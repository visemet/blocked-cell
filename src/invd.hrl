-record(invd, {
    type :: atom()
  , genome :: genome()

  , ga :: pid()
  , index :: array_2d:index()

  , fitness=unknown :: fitness()
  , optimal=min :: 'min' | 'max'

  , stage
}).

%% ----------------------------------------------------------------- %%

-type fitness() :: 'unknown' | number().

-type genome() :: term().

%% ----------------------------------------------------------------- %%
