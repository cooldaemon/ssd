-module(ssd_app).
-author('cooldaemon@gmail.com').
-behaviour(application).

-export([start/2, stop/1]). 

start(_Type, _Args) ->
  Args = get_envs([port, type, yaws_dir, ssd_dir]),
  case application:get_env(ssd, mode) of
    {ok, start} ->
      ssd_sup:start_link(Args);
    {ok, copy} ->
      {ok, Node} = application:get_env(ssd, node),
      ssd_sup:start_link([Node | Args])
  end.

get_envs(Names) ->
  lists:map(fun (Name) ->
    {ok, Value} = application:get_env(ssd, Name),
    Value
  end, Names).

stop(_State) -> ok.

