# competitive_set rejects non-spec elements

    Code
      competitive_set(1, 2)
    Condition
      Error:
      ! <y2conjoint::competitive_set> object is invalid:
      - @specs must contain only spec objects

# competitive_set prints named and unnamed specs

    Code
      print(cs)
    Output
      <competitive_set> Launch: 3 specs: A, B, and 1 unnamed spec
      
        <spec> A
        * Brand: Northwind
        * Price: $249
      
        <spec> B
        * Brand: Cascade
        * Price: $299
      
        <spec> (unnamed)
        * Brand: Northwind
        * Price: $299

# competitive_set rejects specs over different collections

    Code
      competitive_set(spec(brand_only, "Northwind"), spec(price_only, "$249"))
    Condition
      Error:
      ! <y2conjoint::competitive_set> object is invalid:
      - all specs must reference the same collections

