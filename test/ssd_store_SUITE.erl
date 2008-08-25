-module(ssd_store_SUITE).
-author('cooldaemon@gmail.com').

-compile(export_all).

-define(KEY,     "/foo/bar.txt").
-define(CONTENT, "baz").
-define(TYPE,    "text/plain").
-define(SIZE,    3).

all() -> [sync, async].

init_per_suite(Config) ->
  ssd_store:start(ram_copies),
  Config.

end_per_suite(_Config) ->
  ssd_store:stop(),
  ok.

sync() -> [].
sync(_Config) ->
  set_and_delete(
    fun ssd_store:set/4, fun ssd_store:delete/1,
    fun () -> ok end
  ).

async() -> [].
async(_Config) ->
  set_and_delete(
    fun ssd_store:async_set/4, fun ssd_store:async_delete/1,
    fun () -> timer:sleep(250) end
  ).

set_and_delete(SetFunction, DeleteFunction, SleepFunction) ->
  ok = SetFunction(?KEY, ?CONTENT, ?TYPE, ?SIZE),
  SleepFunction(),
  {?CONTENT, ?TYPE, ?SIZE} = ssd_store:get(?KEY),
  ok = DeleteFunction(?KEY),
  SleepFunction(),
  noexists = ssd_store:get(?KEY),
  ok.

