-module(invd).
-behaviour(gen_server).

-export([start/3, evolve/1, get_state/1, send_state/1, receive_state/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("invd.hrl").

-record(init, {}).

-record(evolve, {
    neighbors=[]
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
    pass %TODO:implement

  , io:format("neighbors ~p~n", [neighbors(GA, Index)])

  , {noreply, State#invd{stage=#evolve{}}}
;

handle_cast({send_state, Invd}, State = #invd{}) ->
    receive_state(Invd, State)

  , {noreply, State}
;

handle_cast(
    {receive_state, NeighborState}
  , State = #invd{
        stage = Stage = #evolve{neighbors = Neighbors}
    }
) ->
    {
        noreply
      , State#invd{
            stage=Stage#evolve{neighbors=[NeighborState|Neighbors]}
        }
    }
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

neighbors(GA, Index = {Row, Column})
  when
    is_pid(GA)
  , is_integer(Row), Row >= 0
  , is_integer(Column), Column >= 0
  ->
    lists:map(
        fun (Invd) when is_pid(Invd) ->
            invd:get_state(Invd) % this could cause deadlock
        end

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
