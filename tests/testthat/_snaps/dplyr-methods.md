# overwriting a protected column errors

    Code
      dplyr::mutate(cjt, Northwind = 0)
    Condition
      Error in `dplyr::mutate()`:
      x Can't overwrite the protected column Northwind.
      ! Level and NONE columns of a <conjoint_df> are protected.
      i Convert with `tibble::as_tibble()` first to edit them freely.

# dropping a protected column via select errors

    Code
      dplyr::select(cjt, age)
    Condition
      Error in `dplyr::select()`:
      x Can't drop or rename the protected column Northwind, Cascade, $249, $299, and NONE.
      ! Level and NONE columns of a <conjoint_df> are protected.
      i Convert with `tibble::as_tibble()` first to edit them freely.

# renaming a protected column errors

    Code
      dplyr::rename(cjt, brand = Northwind)
    Condition
      Error in `dplyr::rename()`:
      x Can't drop or rename the protected column Northwind.
      ! Level and NONE columns of a <conjoint_df> are protected.
      i Convert with `tibble::as_tibble()` first to edit them freely.

# errors are attributed to the dplyr verb, not internal helpers

    Code
      dplyr::mutate(cjt, NONE = 0)
    Condition
      Error in `dplyr::mutate()`:
      x Can't overwrite the protected column NONE.
      ! Level and NONE columns of a <conjoint_df> are protected.
      i Convert with `tibble::as_tibble()` first to edit them freely.

---

    Code
      dplyr::rename_with(cjt, toupper)
    Condition
      Error in `dplyr::rename_with()`:
      x Can't drop or rename the protected column Northwind and Cascade.
      ! Level and NONE columns of a <conjoint_df> are protected.
      i Convert with `tibble::as_tibble()` first to edit them freely.

# transmute is unsupported and suggests alternatives

    Code
      dplyr::transmute(cjt, age)
    Condition
      Error in `dplyr::transmute()`:
      x `transmute()` is not supported for a <conjoint_df>.
      ! It would drop the protected level and NONE columns.
      i Use `mutate(..., .keep = 'none')` to keep only new columns.
      i Or call `tibble::as_tibble()` first to drop the structure.

# base assignment guards protected columns

    Code
      cjt$Northwind <- 0
    Condition
      Error in `$<-`:
      x Can't overwrite the protected column Northwind.
      ! Level and NONE columns of a <conjoint_df> are protected.
      i Convert with `tibble::as_tibble()` first to edit them freely.

---

    Code
      cjt[["NONE"]] <- 1
    Condition
      Error in `[[<-`:
      x Can't overwrite the protected column NONE.
      ! Level and NONE columns of a <conjoint_df> are protected.
      i Convert with `tibble::as_tibble()` first to edit them freely.

