# spec warns when a collection has no selected levels

    Code
      s <- spec(cjt, "Northwind")
    Condition
      Warning:
      No levels selected for collection Price.

# spec errors on an unknown level

    Code
      spec(cjt, "Nokia")
    Condition
      Error in `spec()`:
      x Unknown level "Nokia".
      ! Every level must belong to a collection of `x`.
      i Available levels: "Northwind", "Cascade", "$249", and "$299".

