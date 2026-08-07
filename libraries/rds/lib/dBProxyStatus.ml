open Aws.BaseTypes

type t =
  | Available
  | Modifying
  | Incompatible_network
  | Insufficient_resource_limits
  | Creating
  | Deleting
  | Suspended
  | Suspending
  | Reactivating

let str_to_t =
  [ "reactivating", Reactivating
  ; "suspending", Suspending
  ; "suspended", Suspended
  ; "deleting", Deleting
  ; "creating", Creating
  ; "insufficient-resource-limits", Insufficient_resource_limits
  ; "incompatible-network", Incompatible_network
  ; "modifying", Modifying
  ; "available", Available
  ]

let t_to_str =
  [ Reactivating, "reactivating"
  ; Suspending, "suspending"
  ; Suspended, "suspended"
  ; Deleting, "deleting"
  ; Creating, "creating"
  ; Insufficient_resource_limits, "insufficient-resource-limits"
  ; Incompatible_network, "incompatible-network"
  ; Modifying, "modifying"
  ; Available, "available"
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
