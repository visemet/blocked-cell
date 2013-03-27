-record(invd, {
    type :: atom()
  , genome :: genome()

  , ga :: pid()
  , index :: {non_neg_integer(), non_neg_integer()}

  , fitness=unknown :: fitness()
}).

%% ----------------------------------------------------------------- %%

-type fitness() :: 'unknown' | number().

-type genome() :: term().

%% ----------------------------------------------------------------- %%