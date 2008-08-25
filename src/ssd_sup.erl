-module(ssd_sup).
-author('cooldaemon@gmail.com').
-behaviour(supervisor).

-include("ssd.hrl").

% External API
-export([start_link/1, stop/0]). 

% Callbacks
-export([init/1]). 

% External API
start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

stop() ->
  case whereis(?MODULE) of
    Pid when pid(Pid) ->
      exit(Pid, shutdown),
      application:stop(yaws),
      ssd_store:stop(),
      ok;
    _ -> not_started
  end.

% Callbacks
init([Port, Type, YawsDir, SsdDir]) ->
  get_init_result([Port, YawsDir, SsdDir], [Type]);
init([Node, Port, Type, YawsDir, SsdDir]) ->
  get_init_result([Port, YawsDir, SsdDir], [Type, Node]).

% Internal functions
get_init_result(YawsArgs, StoreArgs) ->
  Flags = {one_for_one, 0, 1},
  Children = [
    worker_spec(ssd_store_manager, [StoreArgs]),
    worker_spec(ssd_yaws_manager,  [YawsArgs])
  ],
  {ok, {Flags, Children}}.

worker_spec(Module, Args) ->
  StartFunc = {Module, start_link, Args},
  {Module, StartFunc, permanent, ?SHUTDOWN_WAITING_TIME, worker, [Module]}.

