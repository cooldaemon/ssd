-module(ssd_yaws_mod).
-author('cooldaemon@gmail.com').

-include("yaws_api.hrl").
-export([out/1]).

out(Arg) ->
  Req = Arg#arg.req,
  case Req#http_request.method of
    'HEAD'   -> head_response(Arg);
    'GET'    -> get_response(Arg);
    'PUT'    -> put_response(Arg);
    'DELETE' -> delete_response(Arg);
    _Method  -> other_response(Arg)
  end.

get_path(Arg) ->
  Req = Arg#arg.req,
  {abs_path, Path} = Req#http_request.path,
  Path.

get_type(Arg) ->
  H = Arg#arg.headers,
  case H#headers.content_type of
    undefined -> "test/plain";
    Type      -> Type
  end.

get_length(Arg) ->
  H = Arg#arg.headers,
  case H#headers.content_length of
    undefined -> erlang:length(binary_to_list(Arg#arg.clidata));
    Length    -> Length
  end.

head_response(Arg) ->
  Path = get_path(Arg),
  case ssd_store:get(Path) of
    noexists -> 
      make_response(404, Path ++ " is Not Found.");
    {_Content, Type, Length} ->
      make_response(200, Type, Length, "")
  end.

get_response(Arg) ->
  Path = get_path(Arg),
  case ssd_store:get(Path) of
    noexists ->
      make_response(404, Path ++ " is Not Found.");
    {Content, Type, _Length} ->
      make_response(200, Type, Content)
  end.

put_response(Arg) ->
  ssd_store:async_set(
    get_path(Arg),
    Arg#arg.clidata,
    get_type(Arg),
    get_length(Arg)
  ),
  make_response(202, "Accepted.").

delete_response(Arg) ->
  Path = get_path(Arg),
  ssd_store:async_delete(Path),
  make_response(202, "Accepted.").

other_response(Arg) ->
  Req = Arg#arg.req,
  make_response(
    405,
    yaws_api:f("~s method is not allowed.", [Req#http_request.method])
  ).

make_response(Status, Message) ->
  make_response(Status, "text/plain", Message).

make_response(Status, Type, Message) ->
  make_all_response(Status, make_header(Type), Message).

make_response(Status, Type, Length, Message) ->
  make_all_response(Status, make_header(Type, Length), Message).

make_header(Type) ->
  [{header, ["Content-Type: ", Type]}].

make_header(Type, Length) ->
  [
    {header, ["Content-Type: ",   Type]},
    {header, ["Content-Length: ", Length]}
  ].

make_all_response(Status, Headers, Message) ->
  [
    {status,     Status},
    {allheaders, Headers},
    {html,       Message}
  ].

