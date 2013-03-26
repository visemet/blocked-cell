-record(invd, {
    type :: atom()
  , ga :: pid()
  , fitness=unknown :: fitness()
  , genome :: genome()
}).

%% ----------------------------------------------------------------- %%

-type fitness() :: 'unknown' | number().

-type genome() :: term().

%% ----------------------------------------------------------------- %%
