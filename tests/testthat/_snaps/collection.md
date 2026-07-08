# ordered_collection rejects an order that mismatches levels

    Code
      ordered_collection(name = "Price", levels = c("$249", "$299"), order = c("$249",
        "$399"))
    Condition
      Error:
      ! <y2conjoint::ordered_collection> object is invalid:
      - @order must contain exactly the same levels as @levels

# collection rejects empty levels

    Code
      collection(name = "Brand", levels = character())
    Condition
      Error:
      ! <y2conjoint::collection> object is invalid:
      - @levels must contain at least one level

# collection rejects duplicate levels

    Code
      collection(name = "Brand", levels = c("A", "A"))
    Condition
      Error:
      ! <y2conjoint::collection> object is invalid:
      - @levels must be unique

# collection rejects an empty name

    Code
      collection(name = "", levels = "A")
    Condition
      Error:
      ! <y2conjoint::collection> object is invalid:
      - @name must be a single non-empty string

