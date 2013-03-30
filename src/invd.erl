-module(invd).
-behaviour(gen_server).

-export([start/3, evolve/1, get_state/1, send_state/1, receive_state/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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

-callback evaluate(genome()) -> float().

-callback select([#invd{}]) -> #invd{}.

-callback crossover(genome(), genome()) -> genome().

-callback mutate(genome()) -> genome().

%% ----------------------------------------------------------------- %%

start(Type, Args, Options) when is_atom(Type), is_list(Options) ->
    gen_server:start(?MODULE, [Type, Args, Options], [])
.

evolve(Invd) when is_pid(Invd) ->
    gen_server:cast(Invd, {evolve})
.

get_state(Invd) when is_pid(Invd) ->
    gen_server:call(Invd, {get_state})
.

send_state(Invd) when is_pid(Invd) ->
    gen_server:cast(Invd, {send_state, erlang:self()})
.

receive_state(Invd, State = #invd{}) when is_pid(Invd) ->
    gen_server:cast(Invd, {receive_state, State})
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

handle_call({get_state}, _From, State = #invd{}) ->
    {reply, State, State}
;

handle_call(_Request, _From, State) ->
    {reply, ok, State}
.

handle_cast({evolve}, State = #invd{ga = GA, index = Index}) ->
    Remain = neighbors(GA, Index)

  , {noreply, State#invd{stage=#evolve{remain = Remain}}}
;

handle_cast({send_state, Invd}, State = #invd{}) ->
    receive_state(Invd, State)

  , {noreply, State}
;

handle_cast(
    {receive_state, NeighborState}
  , State = #invd{
        stage = Stage = #evolve{
            neighbors = Neighbors
          , remain = Remain
        }
    }
) ->
    NewRemain = Remain - 1

  , if
        NewRemain =:= 0 ->
            % Ready to evolve
            gen_server:cast(erlang:self(), {evolve_ready})

      ; NewRemain =/= 0 ->
            pass
    end

  , {
        noreply
      , State#invd{
            stage=Stage#evolve{
                neighbors=[NeighborState|Neighbors]
              , remain=NewRemain
            }
        }
    }
;

handle_cast(
    {evolve_ready}
  , State = #invd{
        type = Type
      , fitness = Fitness
      , stage = #evolve{neighbors = Neighbors}
    }
) ->
    io:format("neighbors ~p~n", [Neighbors])

  , Parent1 = Type:select(Neighbors)
  , Parent2 = Type:select(Neighbors)

  , Child = Type:mutate(Type:crossover(Parent1, Parent2))
  , ChildFitness = Type:evaluate(Child)

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
    io:format("other request ~p~n", [_Request])

  , {noreply, State}
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

neighbors(GA, Index = {Row, Column})
  when
    is_pid(GA)
  , is_integer(Row), Row >= 0
  , is_integer(Column), Column >= 0
  ->
    lists:foldl(
        fun (Invd, Count) when is_pid(Invd) ->
            invd:send_state(Invd)

          , Count + 1
        end

      , 0
      , ga:neighbors(GA, Index)
    )
.

send_genome(SenderInvd, ReceiverInvd) ->
    pass
.

receive_genome(ReceiverInvd) ->
    pass
.

%% ----------------------------------------------------------------- %%
