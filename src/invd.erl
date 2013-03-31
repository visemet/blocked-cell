-module(invd).
-behaviour(gen_server).

-export([start/3, start/4, evolve/1]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
  , code_change/3
]).

-include("invd.hrl").

-record(init, {}).

-record(evolve, {
    neighbors=[] :: list()
  , remain=0 :: non_neg_integer()
}).

-record(crossover, {
    parent_a :: 'undefined' | genome()
  , parent_b :: 'undefined' | genome()
}).

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

-callback init(Args :: term()) ->
    {ok, Genome :: genome()}
  | {error, Reason :: term()}
.

-callback evaluate(genome()) -> number().

-callback select([{pid(), number()}]) -> pid().

-callback crossover(genome(), genome()) -> genome().

-callback mutate(genome()) -> genome().

%% ----------------------------------------------------------------- %%

start(Type, Args, Options) when is_atom(Type), is_list(Options) ->
    start(erlang:now(), Type, Args, Options)
.

start(Seed = {MegaSecs, Secs, MicroSecs}, Type, Args, Options)
  when
    is_integer(MegaSecs), MegaSecs >= 0
  , is_integer(Secs), Secs >= 0
  , is_integer(MicroSecs), MicroSecs >= 0
  ->
    gen_server:start(?MODULE, [Seed, Type, Args, Options], [])
.

evolve(Invd) when is_pid(Invd) ->
    gen_server:cast(Invd, {evolve})
.

%% ----------------------------------------------------------------- %%

init([Seed = {MegaSecs, Secs, MicroSecs}, Type, Args, Options])
  when
    is_integer(MegaSecs), MegaSecs >= 0
  , is_integer(Secs), Secs >= 0
  , is_integer(MicroSecs), MicroSecs >= 0
  , is_atom(Type)
  , is_list(Options)
  ->
    random:seed(Seed)

  , case Type:init(Args) of
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

handle_cast({send_genome, Invd}, State = #invd{genome = Genome})
  when
    is_pid(Invd)
  ->
    gen_server:cast(Invd, {receive_genome, Genome})

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
    {receive_genome, Genome}
  , State = #invd{
        stage = Stage = #crossover{
            parent_a = undefined
          , parent_b = undefined
        }
    }
) ->
    NewState = State#invd{
        stage=Stage#crossover{
            parent_a=Genome
        }
    }

  , {noreply, NewState}
;

handle_cast(
    {receive_genome, Genome}
  , State = #invd{
        stage = Stage = #crossover{
            parent_b = undefined
        }
    }
) ->
    gen_server:cast(erlang:self(), {do_crossover})

  , NewState = State#invd{
        stage=Stage#crossover{
            parent_b=Genome
        }
    }

  , {noreply, NewState}
;

handle_cast(
    {do_evolve}
  , State = #invd{
        type = Type
      , stage = #evolve{neighbors = Neighbors, remain = 0}
    }
) ->
    ParentA = Type:select(Neighbors)
  , ParentB = Type:select(Neighbors)

  , send_genome(ParentA)
  , send_genome(ParentB)

  , NewState = State#invd{
        stage=#crossover{}
    }

  , {noreply, NewState}
;

handle_cast(
    {do_crossover}
  , State = #invd{
        type = Type
      , fitness = Fitness
      , optimal = Optimal
      , stage = #crossover{parent_a = ParentA, parent_b = ParentB}
    }
) ->
    Child = Type:mutate(Type:crossover(ParentA, ParentB))
  , ChildFitness = Type:evaluate(Child)

  , io:format("child ~p (fitness ~.4f)~n", [Child, ChildFitness])
  , io:format("self ~p (fitness ~.4f)~n", [State#invd.genome, Fitness])

  % , evolve(erlang:self())

  , NewState = if
        (Optimal =:= min andalso Fitness < ChildFitness)
      orelse (Optimal =:= max andalso Fitness > ChildFitness) ->
            State

      ; (Optimal =:= min andalso Fitness >= ChildFitness)
      orelse (Optimal =:= max andalso Fitness =< ChildFitness) ->
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

init([{optimal, Optimal} | Options], State = #invd{})
  when
    Optimal =:= min orelse Optimal =:= max
  ->
    init(Options, State#invd{optimal=Optimal})
;

init([{seed, Seed = {MegaSecs, Secs, MicroSecs}} | Options], State = #invd{})
  when
    is_integer(MegaSecs), MegaSecs >= 0
  , is_integer(Secs), Secs >= 0
  , is_integer(MicroSecs), MicroSecs >= 0
  ->
    init(options, State)
;

init([Term | _Options], #invd{}) ->
    {error, {badarg, Term}}
.

%% ----------------------------------------------------------------- %%

send_fitness(Invd) when is_pid(Invd) ->
    gen_server:cast(Invd, {send_fitness, erlang:self()})
.

send_genome(Invd) when is_pid(Invd) ->
    gen_server:cast(Invd, {send_genome, erlang:self()})
.

%% ----------------------------------------------------------------- %%

signal_neighbors(GA, Index = {Row, Col})
  when
    is_pid(GA)
  , is_integer(Row), Row >= 0
  , is_integer(Col), Col >= 0
  ->
    lists:foldl(
        fun (Invd, Count) when is_pid(Invd) ->
            send_fitness(Invd)

          , Count + 1
        end

      , 0
      , ga:get_neighbors(GA, Index)
    )
.

%% ----------------------------------------------------------------- %%
