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

# spec errors on a duplicated level

    Code
      spec(cjt, c("Northwind", "Northwind", "$199"))
    Condition
      Error in `spec()`:
      x Level "Northwind" is selected more than once.
      ! Each level may appear at most once in `levels`.

