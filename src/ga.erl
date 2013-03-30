-module(ga).
-behaviour(gen_server).

-export([start/3, neighbors/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(ga, {type, gen_no, invds}).

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

-callback get_neighbors({integer(), integer()}) -> [{integer(), integer()}].

-callback should_terminate(#ga{}) -> boolean().

%% ----------------------------------------------------------------- %%

start(Size, GAType, InvdType) ->
    gen_server:start(?MODULE, [Size, GAType, InvdType], [])
.

neighbors(GA, Coord = {RowIndex, ColumnIndex})
  when
    is_pid(GA)
  , is_integer(RowIndex), RowIndex >= 0
  , is_integer(ColumnIndex), ColumnIndex >= 0
  ->
    gen_server:call(GA, {neighbors, Coord})
.

%% ----------------------------------------------------------------- %%

init([Size = {Rows, Columns}, GAType, InvdType])
  when
    is_integer(Rows), Rows > 0
  , is_integer(Columns), Columns > 0
  , is_atom(GAType)
  , is_atom(InvdType)
  ->
    State = #ga{
        type=GAType
      , gen_no=0
      , invds=array_2d:map(
            fun ({RowIndex, ColumnIndex}, undefined)
              when
                is_integer(RowIndex), RowIndex >= 0
              , is_integer(ColumnIndex), ColumnIndex >= 0
              ->
                {ok, Pid} = invd:start(
                    InvdType
                  , []
                  , [{ga, erlang:self()}, {index, {RowIndex, ColumnIndex}}]
                )

              , io:format("pid ~p~n", [Pid])

              , Pid
            end

          , array_2d:new([
                {size, Size}
            ])
        )
    }

  , {ok, State}
.

handle_call(
    {neighbors, {RowIndex, ColumnIndex}}
  , _From
  , State = #ga{type = Type, invds = Invds}
) when
    is_integer(RowIndex), RowIndex >= 0
  , is_integer(ColumnIndex), ColumnIndex >= 0
  ->
    Neighbors = lists:map(
        fun ({NRowIndex, NColumnIndex})
          when
            is_integer(NRowIndex), NRowIndex >= 0
          , is_integer(NColumnIndex), NColumnIndex >= 0
          ->
            array_2d:get({NRowIndex, NColumnIndex}, Invds)
        end

      , Type:get_neighbors({RowIndex, ColumnIndex})
    )

  , {reply, Neighbors, State}
;

handle_call(_Request, _From, State) ->
    {reply, ok, State}
.

handle_cast(_Request, State) ->
    {noreply, State}
.

handle_info(_Info, State) ->
    {noreply, State}
.

terminate(_Reason, _State) ->
    ok
.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}
.

%%% =============================================================== %%%
%%%  private functions                                              %%%
%%% =============================================================== %%%



%% ----------------------------------------------------------------- %%
