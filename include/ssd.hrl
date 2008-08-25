-author('cooldaemon@gmail.com').

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).
-define(SHUTDOWN_WAITING_TIME, 2000).

-define(ELOG(Message, Args), error_logger:error_msg(
  "node: ~p~nmodule: ~p~nline: ~p~n" ++ Message,
  [node(), ?MODULE, ?LINE | Args]
)).

-record(store, {path, content, type, length}).

