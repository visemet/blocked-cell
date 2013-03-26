-module(select).

-export([tournament/2]).

-include("invd.hrl").

-record(tournament, {order, prob}).

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

tournament(Invds, Options)
  when
    is_list(Invds)
  , is_list(Options)
  ->
    tournament(Invds, Options, #tournament{})
.

%%% =============================================================== %%%
%%%  private functions                                              %%%
%%% =============================================================== %%%

tournament(Invds, [], #tournament{order = Order, prob = Prob}) ->
    tournament_select(Prob, sort_by_fitness(Order, Invds))
;

tournament(Invds, [{order, Order} | Options], State = #tournament{}) ->
    tournament(Invds, Options, State#tournament{order=Order})
;

tournament(Invds, [{prob, Prob} | Options], State = #tournament{}) ->
    tournament(Invds, Options, State#tournament{prob=Prob})
.

tournament_select(_Prob, [Invd]) ->
    Invd
;

tournament_select(Prob, [Invd | Invds]) ->
    Random = random:uniform()
  , if
      Random < Prob -> Invd
    ; Random >= Prob -> tournament_select(Prob * (1 - Prob), Invds)
    end
.

%% ----------------------------------------------------------------- %%

sort_by_fitness(ltg, Invds) ->
    lists:sort(
        fun (#invd{fitness = Fitness1}, #invd{fitness = Fitness2}) ->
            Fitness1 =< Fitness2
        end

      , Invds
    )
;

sort_by_fitness(gtl, Invds) ->
    lists:sort(
        fun (#invd{fitness = Fitness1}, #invd{fitness = Fitness2}) ->
            Fitness1 >= Fitness2
        end

      , Invds
    )
.

%% ----------------------------------------------------------------- %%
