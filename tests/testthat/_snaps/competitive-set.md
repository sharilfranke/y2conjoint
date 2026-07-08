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
        * Price: $199
        * Battery: 20 hours
        * Storage: 256 GB
        * Color: Black
      
        <spec> B
        * Brand: Cascade
        * Price: $299
        * Battery: 10 hours
        * Storage: 128 GB
        * Color: Blue
      
        <spec> (unnamed)
        * Brand: Meridian
        * Price: $399
        * Battery: 30 hours
        * Storage: 512 GB
        * Color: Silver

# competitive_set rejects specs over different collections

    Code
      competitive_set(spec(brand_only, "Northwind"), spec(price_only, "$249"))
    Condition
      Error:
      ! <y2conjoint::competitive_set> object is invalid:
      - all specs must reference the same collections

