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

tournament_select(_Prob, [{Invd, _Fitness}]) ->
    Invd
;

tournament_select(Prob, [{Invd, _Fitness} | Rest]) ->
    Random = random:uniform()
  , if
      Random < Prob -> Invd
    ; Random >= Prob -> tournament_select(Prob * (1 - Prob), Rest)
    end
.

%% ----------------------------------------------------------------- %%

sort_by_fitness(ltg, Invds) ->
    lists:sort(
        fun ({_InvdA, FitnessA}, {_InvdB, FitnessB}) ->
            FitnessA =< FitnessB
        end

      , Invds
    )
;

sort_by_fitness(gtl, Invds) ->
    lists:sort(
        fun ({_InvdA, FitnessA}, {_InvdB, FitnessB}) ->
            FitnessA >= FitnessB
        end

      , Invds
    )
.

%% ----------------------------------------------------------------- %%
