# overwriting a protected column errors

    Code
      dplyr::mutate(cjt, Northwind = 0)
    Condition
      Error in `dplyr::mutate()`:
      x Can't overwrite the protected column: Northwind.
      ! The NONE column and columns that are part of a collection in a <conjoint_df> are protected.
      i Convert to a tibble with `tibble::as_tibble()` to edit them.

# dropping a protected column via select errors

    Code
      dplyr::select(cjt, age)
    Condition
      Error in `dplyr::select()`:
      x Can't drop or rename the protected column: Northwind, Cascade, Meridian, $199, $299, $399, 10 hours, 20 hours, 30 hours, 128 GB, 256 GB, 512 GB, Black, Silver, Blue, and NONE.
      ! The NONE column and columns that are part of a collection in a <conjoint_df> are protected.
      i Convert to a tibble with `tibble::as_tibble()` to edit them.

# renaming a protected column errors

    Code
      dplyr::rename(cjt, brand = Northwind)
    Condition
      Error in `dplyr::rename()`:
      x Can't drop or rename the protected column: Northwind.
      ! The NONE column and columns that are part of a collection in a <conjoint_df> are protected.
      i Convert to a tibble with `tibble::as_tibble()` to edit them.

# errors are attributed to the dplyr verb, not internal helpers

    Code
      dplyr::mutate(cjt, NONE = 0)
    Condition
      Error in `dplyr::mutate()`:
      x Can't overwrite the protected column: NONE.
      ! The NONE column and columns that are part of a collection in a <conjoint_df> are protected.
      i Convert to a tibble with `tibble::as_tibble()` to edit them.

---

    Code
      dplyr::rename_with(cjt, toupper)
    Condition
      Error in `dplyr::rename_with()`:
      x Can't drop or rename the protected column: Northwind, Cascade, Meridian, 10 hours, 20 hours, 30 hours, Black, Silver, and Blue.
      ! The NONE column and columns that are part of a collection in a <conjoint_df> are protected.
      i Convert to a tibble with `tibble::as_tibble()` to edit them.

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
      x Can't overwrite the protected column: Northwind.
      ! The NONE column and columns that are part of a collection in a <conjoint_df> are protected.
      i Convert to a tibble with `tibble::as_tibble()` to edit them.

---

    Code
      cjt[["NONE"]] <- 1
    Condition
      Error in `[[<-`:
      x Can't overwrite the protected column: NONE.
      ! The NONE column and columns that are part of a collection in a <conjoint_df> are protected.
      i Convert to a tibble with `tibble::as_tibble()` to edit them.

