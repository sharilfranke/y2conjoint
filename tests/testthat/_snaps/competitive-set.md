# competitive_set rejects non-spec elements

    Code
      competitive_set(1, 2)
    Condition
      Error:
      ! <y2conjoint::competitive_set> object is invalid:
      - @specs must contain only spec objects

# competitive_set rejects specs over different collections

    Code
      competitive_set(spec(brand_only, "Northwind"), spec(price_only, "$249"))
    Condition
      Error:
      ! <y2conjoint::competitive_set> object is invalid:
      - all specs must reference the same collections

