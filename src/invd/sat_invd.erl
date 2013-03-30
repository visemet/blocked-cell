-module(sat_invd).
-behaviour(invd).

-export([init/1, evaluate/1, select/1, crossover/2, mutate/1]).

-record(sat, {vars=[]}).

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

init(Args) ->
    init(Args, #sat{})
.

evaluate(_Genome = #sat{}) ->
    -1
.

select([{Invd, _Fitness} | _Rest]) ->
    Invd
.

crossover(GenomeA = #sat{}, _GenomeB = #sat{}) ->
    GenomeA
.

mutate(Genome = #sat{}) ->
    Genome
.

%%% =============================================================== %%%
%%%  private functions                                              %%%
%%% =============================================================== %%%

init([], State = #sat{}) ->
    {ok, State}
;

init([{var_names, VarNames} | Args], State = #sat{}) ->
    Vars = lists:zip(
        VarNames
      , utils:random(0, 1, erlang:length(VarNames))
    )

  , init(Args, State#sat{vars=Vars})
;

init([Term, _Args], #sat{}) ->
    {error, {badarg, Term}}
.

%% ----------------------------------------------------------------- %%
