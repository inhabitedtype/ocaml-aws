open Aws.BaseTypes

type t =
  | Active
  | Not_in_use
  | Enabled
  | Disabled
  | Enabling
  | Disabling
  | Modifying_max_capacity
  | Error

let str_to_t =
  [ "error", Error
  ; "modifying-max-capacity", Modifying_max_capacity
  ; "disabling", Disabling
  ; "enabling", Enabling
  ; "disabled", Disabled
  ; "enabled", Enabled
  ; "not-in-use", Not_in_use
  ; "active", Active
  ]

let t_to_str =
  [ Error, "error"
  ; Modifying_max_capacity, "modifying-max-capacity"
  ; Disabling, "disabling"
  ; Enabling, "enabling"
  ; Disabled, "disabled"
  ; Enabled, "enabled"
  ; Not_in_use, "not-in-use"
  ; Active, "active"
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
