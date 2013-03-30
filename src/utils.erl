-module(utils).

-export([random/3]).

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

random(Min, Max, Length)
  when
    is_integer(Min), Min >= 0
  , is_integer(Max), Max >= Min
  , is_integer(Length), Length >= 0
  ->
    Range = Max - Min + 1
  , Offset = Min - 1

  , random(Range, Offset, Length, [])
.

%%% =============================================================== %%%
%%%  private functions                                              %%%
%%% =============================================================== %%%

random(_Range, _Offset, 0, Result)
  when
    is_list(Result)
  ->
    lists:reverse(Result)
;

random(Range, Offset, Length, Result)
  when
    is_integer(Range), Range >= 1
  , is_integer(Offset)
  , is_integer(Length), Length > 0
  , is_list(Result)
  ->
    random(
        Range
      , Offset
      , Length - 1
      , [random:uniform(Range) + Offset|Result]
    )
.

%% ----------------------------------------------------------------- %%
