-module(sat_invd).
-behaviour(invd).

-export([init/1, evaluate/1, select/1, crossover/2, mutate/1]).

-include("sat_invd.hrl").

-record(sat, {vars=[], formula=1}).

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

init(Args) ->
    init(Args, #sat{})
.

evaluate(_Genome = #sat{vars = Vars, formula = Formula}) ->
    % Substituted = substitute(dict:from_list(Vars), Formula)
    evaluate(dict:from_list(Vars), Formula)
.

select([{Invd, _Fitness} | _Rest]) ->
    Invd
.

crossover(GenomeA = #sat{vars = VarsA}, _GenomeB = #sat{vars = VarsB}) ->
    Length = erlang:length(VarsA)

  , [Point1, Point2] = lists:sort(utils:random(1, Length, 2))

  , NewVars = lists:zipwith3(
        fun (LiteralA, LiteralB, Index) ->
            if
                Index < Point1 -> LiteralA

              ; Index >= Point1, Index < Point2 -> LiteralB

              ; Index >= Point2 -> LiteralA
            end
        end

      , VarsA
      , VarsB
      , lists:seq(1, Length)
    )

  , GenomeA#sat{vars=NewVars}
.

mutate(Genome = #sat{vars = Vars}) ->
    Prob = erlang:length(Vars)

  , NewVars = lists:map(
        fun (Literal = {VarName, Value}) ->
            Random = random:uniform()

          , if
                Value =:= 0, Random =< Prob -> {VarName, 1}

              ; Value =:= 1, Random =< Prob -> {VarName, 0}

              ; Random > Prob -> Literal
            end
        end

      , Vars
    )

  , Genome#sat{vars=NewVars}
.

%%% =============================================================== %%%
%%%  private functions                                              %%%
%%% =============================================================== %%%

init([], State = #sat{}) ->
    {ok, State}
;

init([{var_names, VarNames} | Args], State = #sat{})
  when
    is_list(VarNames)
  , is_list(Args)
  ->
    Vars = lists:zip(
        VarNames
      , utils:random(0, 1, erlang:length(VarNames))
    )

  , init(Args, State#sat{vars=Vars})
;

init([{formula, Formula} | Args], State = #sat{}) when is_list(Args) ->
    init(Args, State#sat{formula=Formula})
;

init([Term, _Args], #sat{}) ->
    {error, {badarg, Term}}
.

%% ----------------------------------------------------------------- %%

substitute(_Vars, 0) ->
    0
;

substitute(_Vars, 1) ->
    1
;

substitute(Vars, Literal) when is_atom(Literal) ->
    dict:fetch(Literal, Vars)
;

substitute(Vars, Formula = #'and'{clauses = Clauses}) ->
    NewClauses = lists:map(
        fun (Clause) ->
            substitute(Vars, Clause)
        end

      , Clauses
    )

  , Formula#'and'{clauses=NewClauses}
;

substitute(Vars, Formula = #'or'{clauses = Clauses}) ->
    NewClauses = lists:map(
        fun (Clause) ->
            substitute(Vars, Clause)
        end

      , Clauses
    )

  , Formula#'or'{clauses=NewClauses}
;

substitute(Vars, Formula = #'not'{clause = Clause}) ->
    NewClause = substitute(Vars, Clause)

  , Formula#'not'{clause=NewClause}
.

%% ----------------------------------------------------------------- %%

evaluate(_Vars, 0) ->
    0
;

evaluate(_Vars, 1) ->
    1
;

evaluate(Vars, Literal) when is_atom(Literal) ->
    dict:fetch(Literal, Vars)
;

evaluate(Vars, #'and'{clauses = Clauses}) ->
    Count = erlang:length(Clauses)

  , Sum = lists:foldl(
        fun (Clause, Acc) ->
            Value = evaluate(Vars, Clause)

          , Acc + Value
        end

      , 0
      , Clauses
    )

  , Sum / Count
;

evaluate(Vars, #'or'{clauses = Clauses}) ->
    lists:foldl(
        fun (Clause, Acc) ->
            Value = evaluate(Vars, Clause)

          , if
                Value > Acc -> Value

              ; Value =< Acc -> Acc
            end
        end

      , 0
      , Clauses
    )
;

evaluate(Vars, #'not'{clause = Clause}) ->
    1 - evaluate(Vars, Clause)
.

%% ----------------------------------------------------------------- %%
