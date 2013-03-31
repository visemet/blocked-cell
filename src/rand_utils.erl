-module(rand_utils).

-export([new_seed/0, uniform/3]).

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

new_seed() ->
    erlang:list_to_tuple(
        uniform(0, 100000, 3)
    )
.

uniform(Min, Max, Length)
  when
    is_integer(Min), Min >= 0
  , is_integer(Max), Max >= Min
  , is_integer(Length), Length >= 0
  ->
    Range = Max - Min + 1
  , Offset = Min - 1

  , uniform(Range, Offset, Length, [])
.

%%% =============================================================== %%%
%%%  private functions                                              %%%
%%% =============================================================== %%%

uniform(_Range, _Offset, 0, Result)
  when
    is_list(Result)
  ->
    lists:reverse(Result)
;

uniform(Range, Offset, Length, Result)
  when
    is_integer(Range), Range >= 1
  , is_integer(Offset)
  , is_integer(Length), Length > 0
  , is_list(Result)
  ->
    uniform(
        Range
      , Offset
      , Length - 1
      , [random:uniform(Range) + Offset|Result]
    )
.

%% ----------------------------------------------------------------- %%
