-module(ssd_ctl).
-author('cooldaemon@gmail.com').

-export([start/0, copy/0, stop/0, change_copy_type/0, show/0]).

start() -> app_start(start).
copy()  -> app_start(copy).

app_start(Mode) ->
  case ssd_argument:get(Mode, daemon) of
    error ->
      init:stop(),
      error;
    Options ->
      BaseNames = [port, type, yaws_dir, ssd_dir],
      Names = case Mode of
        start -> BaseNames;
        _     -> [node | BaseNames]
      end,
      lists:foreach(get_setenv_fun(Options), Names),
      application:set_env(ssd, mode, Mode),
      application:start(ssd, permanent)
  end.

get_setenv_fun(Options) ->
  fun (Name) ->
    application:set_env(
      ssd,
      Name,
      proplists:get_value(Name, Options)
    )
  end.

stop() ->
  case ssd_argument:get(stop) of
    error -> error;
    Options ->
      Node = proplists:get_value(node, Options),
      put_message(rpc:call(Node, init, stop, []), "Stopping.")
  end.

change_copy_type() ->
  case ssd_argument:get(chtype) of
    error -> error;
    Options ->
      Node = proplists:get_value(node, Options),
      Type = proplists:get_value(type, Options),
      put_message(
        rpc:call(Node, ssd_store, change_copy_type, [Type]),
        "Changing storage type."
      )
  end.

show() ->
  case ssd_argument:get(show) of
    error -> error;
    Options ->
      Message = proplists:get_value(message, Options),
      rb:start(),
      rb:show(Message)
  end.

put_message(Result, Message) ->
  case Result of
    ok -> io:fwrite("~s~n", [Message]), ok;
    _  -> error
  end.

