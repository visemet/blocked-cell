-module(linear5_ga).
-behaviour(ga).

-export([start/4]).
-export([get_neighbor_coords/2]).

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

start(Size = {NRows, NCols}, Optimal, InvdType, InvdArgs)
  when
    is_integer(NRows), NRows >= 0
  , is_integer(NCols), NCols >= 0
  , Optimal =:= min orelse Optimal =:= max
  , is_atom(InvdType)
  ->
    ga:start(Size, ?MODULE, Optimal, InvdType, InvdArgs)
.

%% ----------------------------------------------------------------- %%

get_neighbor_coords(Index = {Row, Col}, _Size = {NRows, NCols})
  when
    is_integer(Row), Row >= 0
  , is_integer(Col), Col >= 0
  , is_integer(NRows), NRows >= 0, Row < NRows
  , is_integer(NCols), NCols >= 0, Col < NCols
  ->
    North = {Row - 1, Col}
  , East = {Row, Col + 1}
  , South = {Row + 1, Col}
  , West = {Row, Col - 1}

  , lists:filter(
        fun ({R, C}) when is_integer(R), is_integer(C) ->
            if
                R >= 0, R < NRows, C >= 0, C < NCols ->
                  true

              ; true ->
                  false
            end
        end

      , [Index, North, East, South, West]
    )
.

%%% =============================================================== %%%
%%%  private functions                                              %%%
%%% =============================================================== %%%



%% ----------------------------------------------------------------- %%
