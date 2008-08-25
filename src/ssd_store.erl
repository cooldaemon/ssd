-module(ssd_store).
-author('cooldaemon@gmail.com').

-include("ssd.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
  start/1, start/2, stop/0, change_copy_type/1,
  get/1, set/4, async_set/4, delete/1, async_delete/1
]).

start(ram_copies) ->
  case mnesia:system_info(use_dir) of
    true -> schema_exists_on_disc;
    _    -> start_and_create_tables(ram_copies)
  end;

start(disc_copies) ->
  case mnesia:system_info(use_dir) of
    true ->
      mnesia:start(),
      {atomic, ok};
    _ ->
      mnesia:create_schema([node()]),
      start_and_create_tables(disc_copies)
  end.

start_and_create_tables(CopyType) ->
  mnesia:start(),
  create_table(store, record_info(fields, store), CopyType).

create_table(Name, RecordInfo, CopyType) ->
  DiscCopies = case CopyType of
    disc_copies -> [{disc_copies, [node()]}];
    _           -> []
  end,
  mnesia:create_table(
    Name,
    lists:append([{attributes, RecordInfo}], DiscCopies)
  ).

start(CopyType, Node) ->
  case net_adm:ping(Node) of
    pong -> case mnesia:system_info(use_dir) of
              true -> schema_exists_on_disc;
              _    -> start_and_copy_tables(CopyType, Node)
            end;
    _    -> node_noexists
  end.
  
start_and_copy_tables(CopyType, Node) ->
  mnesia:start(),
  mnesia:change_config(extra_db_nodes, [Node]),
  case CopyType of
    disc_copies -> mnesia:change_table_copy_type(schema, node(), CopyType);
    _           -> ok
  end,
  case lists:any(
    fun (store) -> true; (_) -> false end,
    mnesia:system_info(local_tables)
  ) of
    false ->
      mnesia:add_table_copy(store, node(), CopyType);
    _ ->
      change_copy_type(CopyType)
  end.

stop() ->
  mnesia:stop().

change_copy_type(CopyType) ->
  Names = [schema, store],
  SortNames = case CopyType of
    ram_copies -> lists:reverse(Names);
    _          -> Names
  end,
  Node = node(),
  lists:foreach(fun (Name) ->
    case
      lists:any(
        fun (X) -> case Node of X -> true; _ -> false end end,
          mnesia:table_info(Name, CopyType)
      )
    of
      true -> ok;
      _    -> mnesia:change_table_copy_type(Name, node(), CopyType)
    end
  end, SortNames),
  {atomic, ok}.

transaction(Fun) ->
  case mnesia:transaction(Fun) of
    {atomic, Result} -> Result;
    Error ->
      ?ELOG("~p", Error),
      error
  end.

get(Path) ->
  transaction(fun () ->
    case mnesia:read(store, Path, read) of
      [] ->
        noexists;
      [{store, _Path, Content, Type, Length}] ->
        {Content, Type, Length};
      Error ->
        ?ELOG("~p", Error),
        error
    end
  end).

set(Path, Content, Type, Length) ->
  Row = #store{path=Path, content=Content, type=Type, length=Length},
  transaction(fun() -> mnesia:write(Row) end).

async_set(Path, Content, Type, Length) ->
  spawn(?MODULE, set, [Path, Content, Type, Length]),
  ok.

delete(Path) ->
  transaction(fun () -> mnesia:delete({store, Path}) end).

async_delete(Path) ->
  spawn(?MODULE, delete, [Path]),
  ok.

