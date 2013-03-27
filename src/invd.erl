-module(invd).
-behaviour(gen_server).

-export([start/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("invd.hrl").

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

%% ----------------------------------------------------------------- %%

init([Type, Args, Options]) when is_atom(Type), is_list(Options) ->
    Genome = Type:init(Args)

  , init(Options, #invd{type=Type, genome=Genome})
.

handle_call(_Request, _From, State) ->
    {reply, ok, State}
.

handle_cast({evolve}, State = #invd{}) ->
    pass %TODO:implement
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

send_genome(SenderInvd, ReceiverInvd) ->
    pass
.

receive_genome(ReceiverInvd) ->
    pass
.

%% ----------------------------------------------------------------- %%
