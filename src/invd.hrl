-record(invd, {
    type :: atom()
  , genome :: genome()

  , ga :: pid()
  , index :: array_2d:index()

  , fitness=unknown :: fitness()
  , optimal=min :: 'min' | 'max'

  , gen_no=0 :: non_neg_integer()
  , age=1 :: pos_integer()

  , stage
}).

%% ----------------------------------------------------------------- %%

-type fitness() :: 'unknown' | number().

-type genome() :: term().

%% ----------------------------------------------------------------- %%
