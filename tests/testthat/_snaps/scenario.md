# run_scenario rejects combine_fn naming an unknown collection

    Code
      run_scenario(cjt, cs, combine_fn = list(Colour = pmax))
    Condition
      Error in `run_scenario()`:
      x `combine_fn` names collection not found in `x`.
      ! Unknown: Colour.

