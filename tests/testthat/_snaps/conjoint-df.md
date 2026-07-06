# conjoint_df errors when a crosswalk level is missing from data

    Code
      conjoint_df(sample_data(), crosswalk)
    Condition
      Error in `conjoint_df()`:
      x `crosswalk` maps column A9B9 that is not in `data`.
      ! Every old_name must name a column of `data`.

# conjoint_df errors on duplicate user names

    Code
      conjoint_df(sample_data(), crosswalk)
    Condition
      Error in `conjoint_df()`:
      x user_name values in `crosswalk` must be unique.
      ! Duplicated: Northwind.

# conjoint_df errors when the NONE column is absent

    Code
      conjoint_df(data, sample_crosswalk())
    Condition
      Error in `conjoint_df()`:
      x `none` column NONE is not in `data`.
      i Set `none` to the name of your outside-good column.

# conjoint_df errors on a partial collection order

    Code
      conjoint_df(sample_data(), crosswalk)
    Condition
      Error in `conjoint_df()`:
      x Collection Price has a partial collection_order.
      ! Ordered collections need a rank for every level.
      i Give every level an order, or set them all to NA.

# conjoint_df prints a structure header

    Code
      print(sample_conjoint())
    Output
      <conjoint_df>: 2 collections (* = ordered), NONE = NONE, 2 extra columns
      Collections: Brand and Price*
      # A tibble: 3 x 7
           ID Northwind Cascade `$249` `$299`  NONE   age
        <int>     <dbl>   <dbl>  <dbl>  <dbl> <dbl> <dbl>
      1     1       0.2    -0.3    0.1    0     0      30
      2     2      -0.1     0.4    0.2   -0.1   0.1    40
      3     3       0.5     0.1   -0.2    0.4  -0.2    25

