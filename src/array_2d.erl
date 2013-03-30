-module(array_2d).

-export([new/1, size/1, set/3, get/2, map/2, foldl/3]).

-export_type([index/0]).

-record(array_2d, {size = {0, 0}, default, elements}).

%%% =============================================================== %%%
%%%  API                                                            %%%
%%% =============================================================== %%%

-type index() :: {array:array_indx(), array:array_indx()}.

%% ----------------------------------------------------------------- %%

new(Options) when is_list(Options) -> new(Options, #array_2d{}).

size(#array_2d{size = Size}) -> Size.

set(
    {RowIndex, ColumnIndex}
  , Value
  , Array2D = #array_2d{size = {Rows, Columns}, elements = Elements}
) when
    is_integer(RowIndex), RowIndex >= 0
  , is_integer(ColumnIndex), ColumnIndex >= 0
  , is_integer(Rows), RowIndex < Rows
  , is_integer(Columns), ColumnIndex < Columns
->
    Array2D#array_2d{
        elements=array:set(
            ColumnIndex
          , Value
          , array:get(RowIndex, Elements)
        )
    }
.

get(
    {RowIndex, ColumnIndex}
  , #array_2d{size = {Rows, Columns}, elements = Elements}
) when
    is_integer(RowIndex), RowIndex >= 0
  , is_integer(ColumnIndex), ColumnIndex >= 0
  , is_integer(Rows), RowIndex < Rows
  , is_integer(Columns), ColumnIndex < Columns
->
    array:get(
        ColumnIndex
      , array:get(RowIndex, Elements)
    )
.

map(
    Function
  , Array2D = #array_2d{size = {Rows, Columns}, elements = Elements}
) when
    is_function(Function, 2)
  , is_integer(Rows), Rows >=0
  , is_integer(Columns), Columns >= 0
  ->
    Array2D#array_2d{
        elements=array:map(
            fun (RowIndex, Array) when is_integer(RowIndex), RowIndex >= 0 ->
                array:map(
                    fun (ColumnIndex, Value)
                      when
                        is_integer(ColumnIndex), ColumnIndex >= 0
                      ->
                        Function({RowIndex, ColumnIndex}, Value)
                    end

                  , Array
                )
            end

          , Elements
        )
    }
.

foldl(
    Function
  , InitAcc
  , #array_2d{size = {Rows, Columns}, elements = Elements}
) when
    is_function(Function, 3)
  , is_integer(Rows), Rows >= 0
  , is_integer(Columns), Columns >= 0
  ->
    array:foldl(
        fun (RowIndex, Array, RowAcc)
          when
            is_integer(RowIndex), RowIndex >= 0
          ->
            array:foldl(
                fun (ColumnIndex, Value, ColumnAcc)
                  when
                    is_integer(ColumnIndex), ColumnIndex >= 0
                  ->
                    Function({RowIndex, ColumnIndex}, Value, ColumnAcc)
                end

              , RowAcc
              , Array
            )
        end

      , InitAcc
      , Elements
    )
.

%%% =============================================================== %%%
%%%  private functions                                              %%%
%%% =============================================================== %%%

new(
    []
  , Array2D = #array_2d{
        size = {Rows, Columns}
      , default = Default
  }
) when
    is_integer(Rows), Rows >=0
  , is_integer(Columns), Columns >= 0
  ->
    Array2D#array_2d{
        elements=array:map(
            fun (Index, undefined) when is_integer(Index) ->
                array:new([
                    {size, Columns}
                  , {fixed, true}
                  , {default, Default}
                ])
            end

          , array:new([
                {size, Rows}
              , {fixed, true}
            ])
        )
    }
;

new(
    [{size, Size = {Rows, Columns}} | Options]
  , Array2D = #array_2d{}
) when
    is_integer(Rows), Rows >=0
  , is_integer(Columns), Columns >= 0
  , is_list(Options)
  ->
    new(Options, Array2D#array_2d{size=Size})
;

new(
    [{default, Default} | Options]
  , Array2D = #array_2d{}
) when
    is_list(Options)
->
    new(Options, Array2D#array_2d{default=Default})
.

%% ----------------------------------------------------------------- %%
