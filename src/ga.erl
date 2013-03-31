-module(ga).
-behaviour(gen_server).

-export([start/5, start/6, get_neighbors/2]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2
  , terminate/2, code_change/3
]).

-record(ga, {type, invds}).

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

-callback get_neighbor_coords(
    Index :: array_2d:index()
  , Size :: array_2d:index()
) -> [array_2d:index()].

%% ----------------------------------------------------------------- %%

start(Size = {NRows, NCols}, GAType, Optimal, InvdType, InvdArgs)
  when
    is_integer(NRows), NRows >= 0
  , is_integer(NCols), NCols >= 0
  , is_atom(GAType)
  , Optimal =:= min orelse Optimal =:= max
  , is_atom(InvdType)
  ->
    start(erlang:now(), Size, GAType, Optimal, InvdType, InvdArgs)
.

start(
    Seed = {MegaSecs, Secs, MicroSecs}
  , Size = {NRows, NCols}
  , GAType
  , Optimal
  , InvdType
  , InvdArgs
) when
    is_integer(MegaSecs), MegaSecs >= 0
  , is_integer(Secs), Secs >= 0
  , is_integer(MicroSecs), MicroSecs >= 0
  , is_integer(NRows), NRows >= 0
  , is_integer(NCols), NCols >= 0
  , is_atom(GAType)
  , Optimal =:= min orelse Optimal =:= max
  , is_atom(InvdType)
  ->
    gen_server:start(
        ?MODULE
      , [Seed, Size, GAType, Optimal, InvdType, InvdArgs]
      , []
    )
.

get_neighbors(GA, Index = {Row, Col})
  when
    is_pid(GA)
  , is_integer(Row), Row >= 0
  , is_integer(Col), Col >= 0
  ->
    gen_server:call(GA, {get_neighbors, Index})
.

%% ----------------------------------------------------------------- %%

init([
    Seed = {MegaSecs, Secs, MicroSecs}
  , Size = {NRows, NCols}
  , GAType
  , Optimal
  , InvdType
  , InvdArgs
]) when
    is_integer(MegaSecs), MegaSecs >= 0
  , is_integer(Secs), Secs >= 0
  , is_integer(MicroSecs), MicroSecs >= 0
  , is_integer(NRows), NRows >= 0
  , is_integer(NCols), NCols >= 0
  , is_atom(GAType)
  , Optimal =:= min orelse Optimal =:= max
  , is_atom(InvdType)
  ->
    random:seed(Seed)

  , Invds = array_2d:map(
        fun ({Row, Col}, undefined)
          when
            is_integer(Row), Row >= 0
          , is_integer(Col), Col >= 0
          ->
            InvdOptions = [
                {ga, erlang:self()}
              , {index, {Row, Col}}
              , {optimal, Optimal}
            ]

          , {ok, Invd} = invd:start(
                InvdType
              , InvdArgs
              , InvdOptions
            )

          , invd:evolve(Invd)

          , Invd
        end

      , array_2d:new([
            {size, Size}
        ])
    )

  , State = #ga{
        type=GAType
      , invds=Invds
    }

  , {ok, State}
.

handle_call(
    {get_neighbors, {Row, Col}}
  , _From
  , State = #ga{type = Type, invds = Invds}
) when
    is_integer(Row), Row >= 0
  , is_integer(Col), Col >= 0
  ->
    Neighbors = lists:map(
        fun ({R, C})
          when
            is_integer(R), R >= 0
          , is_integer(C), C >= 0
          ->
            array_2d:get({R, C}, Invds)
        end

      , Type:get_neighbor_coords(
            {Row, Col}
          , array_2d:size(Invds)
        )
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
