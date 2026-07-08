# spec warns when a collection has no selected levels

    Code
      s <- spec(cjt, "Northwind")
    Condition
      Warning:
      No levels selected for collections Price, Battery, Storage, and Color.

# spec errors on an unknown level

    Code
      spec(cjt, "Nokia")
    Condition
      Error in `spec()`:
      x Unknown level "Nokia".
      ! Every level must belong to a collection of `x`.
      i Available levels: "Northwind", "Cascade", "Meridian", "$199", "$299", "$399", "10 hours", "20 hours", "30 hours", "128 GB", "256 GB", "512 GB", "Black", "Silver", and "Blue".

