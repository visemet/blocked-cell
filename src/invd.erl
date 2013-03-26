-module(invd).
-behaviour(gen_server).

-export([start/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("invd.hrl").

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

-callback evaluate(genome()) -> float().

-callback select([#invd{}]) -> #invd{}.

-callback crossover(genome(), genome()) -> genome().

-callback mutate(genome()) -> genome().

%% ----------------------------------------------------------------- %%

start(Type, GA) when is_atom(Type), is_pid(GA) ->
    gen_server:start(?MODULE, [Type, GA], [])
.

%% ----------------------------------------------------------------- %%

init([Type, GA]) when is_atom(Type), is_pid(GA) ->
    State = #invd{
        type=Type
      , ga=GA
    }

  , {ok, State}
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

send_genome(SenderInvd, ReceiverInvd) ->
    pass
.

receive_genome(ReceiverInvd) ->
    pass
.

%% ----------------------------------------------------------------- %%
