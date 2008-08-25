-module(ssd_yaws_manager).
-author('cooldaemon@gmail.com').

-include("yaws.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/1]).

%% Callbacks
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

%% External API
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% Callbacks
init(Args) ->
  process_flag(trap_exit, true),
  case application:start(yaws) of
    ok    -> set_conf(Args);
    Error -> {stop, Error}
  end.

set_conf([Port, YawsDir, SsdDir]) ->
  GC = #gconf{
    yaws_dir = YawsDir,
    trace    = false,
    logdir   = SsdDir ++ "/logs",
    ebin_dir = [SsdDir ++ "/ebin"],
    yaws     = "Simple Storage Daemon 1.0 Bate 1",
    tmpdir   = SsdDir ++ "/.yaws"
  },

  SC = #sconf{
    port       = Port,
    servername = "localhost",
    listen     = {0, 0, 0, 0},
    docroot    = "/tmp",
    appmods    = [{"/", ssd_yaws_mod}]
  },

  case catch yaws_api:setconf(GC, [[SC]]) of
    ok    -> {ok, started};
    Error -> {stop, Error}
  end.

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  application:stop(yaws),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

