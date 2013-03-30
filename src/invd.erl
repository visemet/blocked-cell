-module(invd).
-behaviour(gen_server).

-export([start/3, evolve/1, send_fitness/1]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
  , code_change/3
]).

-include("invd.hrl").

-record(init, {}).

-record(evolve, {
    neighbors=[]
  , remain=0
}).

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

-callback init(term()) -> genome().

-callback evaluate(genome()) -> number().

-callback select([{pid(), number()}]) -> pid().

-callback crossover(genome(), genome()) -> genome().

-callback mutate(genome()) -> genome().

%% ----------------------------------------------------------------- %%

start(Type, Args, Options) when is_atom(Type), is_list(Options) ->
    gen_server:start(?MODULE, [Type, Args, Options], [])
.

evolve(Invd) when is_pid(Invd) ->
    gen_server:cast(Invd, {evolve})
.

send_fitness(Invd) when is_pid(Invd) ->
    gen_server:cast(Invd, {send_fitness, erlang:self()})
.

%% ----------------------------------------------------------------- %%

init([Type, Args, Options]) when is_atom(Type), is_list(Options) ->
    case Type:init(Args) of
        {ok, Genome} ->
            init(Options, #invd{type=Type, genome=Genome, stage=#init{}})

      ; {error, Reason} ->
            {error, Reason}
    end
.

handle_call(_Request, _From, State) ->
    {reply, ok, State}
.

handle_cast({evolve}, State = #invd{ga = GA, index = Index}) ->
    NewState = State#invd{
        stage=#evolve{
            neighbors=[]
          , remain=signal_neighbors(GA, Index)
        }
    }

  , {noreply, NewState}
;

handle_cast(
    {send_fitness, Invd}
  , State = #invd{type = Type, genome = Genome, fitness = unknown}
) when
    is_pid(Invd)
  ->
    Fitness = Type:evaluate(Genome)
  , gen_server:cast(Invd, {receive_fitness, {erlang:self(), Fitness}})

  , NewState = State#invd{fitness=Fitness}
  , {noreply, NewState}
;

handle_cast({send_fitness, Invd}, State = #invd{fitness = Fitness})
  when
    is_pid(Invd)
  , is_number(Fitness)
  ->
    gen_server:cast(Invd, {receive_fitness, {erlang:self(), Fitness}})

  , {noreply, State}
;

handle_cast(
    {receive_fitness, Neighbor = {Invd, Fitness}}
  , State = #invd{
        stage = Stage = #evolve{
            neighbors = Neighbors
          , remain = 1
        }
    }
) when
    is_pid(Invd)
  , is_number(Fitness)
  ->
    gen_server:cast(erlang:self(), {do_evolve})

  , NewState = State#invd{
        stage=Stage#evolve{
            neighbors=[Neighbor|Neighbors]
          , remain=0
        }
    }

  , {noreply, NewState}
;

handle_cast(
    {receive_fitness, Neighbor = {Invd, Fitness}}
  , State = #invd{
        stage = Stage = #evolve{
            neighbors = Neighbors
          , remain = Remain
        }
    }
) when
    is_pid(Invd)
  , is_number(Fitness)
  ->
    NewState = State#invd{
        stage=Stage#evolve{
            neighbors=[Neighbor|Neighbors]
          , remain=Remain - 1
        }
    }

  , {noreply, NewState}
;

handle_cast(
    {do_evolve}
  , State = #invd{
        type = Type
      , fitness = Fitness
      , stage = #evolve{neighbors = Neighbors, remain = 0}
    }
) ->
    io:format("neighbors ~p~n", [Neighbors])

  , ParentA = Type:select(Neighbors)
  , ParentB = Type:select(Neighbors)

  , Child = Type:mutate(Type:crossover(ParentA, ParentB))
  , ChildFitness = Type:evaluate(Child)

  % , evolve(erlang:self())

    % TODO: allow specification of minimize or maximize
  , NewState = if
        Fitness > ChildFitness ->
            State

      ; Fitness =< ChildFitness ->
            State#invd{
                genome=Child
              , fitness=ChildFitness
            }
    end

  , {noreply, NewState}
;

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

init([], State = #invd{}) ->
    {ok, State}
;

init([{ga, GA} | Options], State = #invd{})
  when
    is_pid(GA)
  , is_list(Options)
  ->
    init(Options, State#invd{ga=GA})
;

init([{index, Index = {Row, Column}} | Options], State = #invd{})
  when
    is_integer(Row), Row >= 0
  , is_integer(Column), Column >= 0
  , is_list(Options)
  ->
    init(Options, State#invd{index=Index})
;

init([Term | _Options], #invd{}) ->
    {error, {badarg, Term}}
.

signal_neighbors(GA, Index = {Row, Col})
  when
    is_pid(GA)
  , is_integer(Row), Row >= 0
  , is_integer(Col), Col >= 0
  ->
    lists:foldl(
        fun (Invd, Count) when is_pid(Invd) ->
            invd:send_fitness(Invd)

          , Count + 1
        end

      , 0
      , ga:get_neighbors(GA, Index)
    )
.

%% ----------------------------------------------------------------- %%
