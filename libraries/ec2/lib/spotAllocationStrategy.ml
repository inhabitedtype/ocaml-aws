open Aws.BaseTypes

type t =
  | Lowest_price
  | Diversified
  | Capacity_optimized
  | Capacity_optimized_prioritized
  | Price_capacity_optimized

let str_to_t =
  [ "price-capacity-optimized", Price_capacity_optimized
  ; "capacity-optimized-prioritized", Capacity_optimized_prioritized
  ; "capacity-optimized", Capacity_optimized
  ; "diversified", Diversified
  ; "lowest-price", Lowest_price
  ]

let t_to_str =
  [ Price_capacity_optimized, "price-capacity-optimized"
  ; Capacity_optimized_prioritized, "capacity-optimized-prioritized"
  ; Capacity_optimized, "capacity-optimized"
  ; Diversified, "diversified"
  ; Lowest_price, "lowest-price"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
