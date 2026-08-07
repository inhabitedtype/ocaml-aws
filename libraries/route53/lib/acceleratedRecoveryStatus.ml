open Aws.BaseTypes

type t =
  | ENABLING
  | ENABLE_FAILED
  | ENABLING_HOSTED_ZONE_LOCKED
  | ENABLED
  | DISABLING
  | DISABLE_FAILED
  | DISABLED
  | DISABLING_HOSTED_ZONE_LOCKED

let str_to_t =
  [ "DISABLING_HOSTED_ZONE_LOCKED", DISABLING_HOSTED_ZONE_LOCKED
  ; "DISABLED", DISABLED
  ; "DISABLE_FAILED", DISABLE_FAILED
  ; "DISABLING", DISABLING
  ; "ENABLED", ENABLED
  ; "ENABLING_HOSTED_ZONE_LOCKED", ENABLING_HOSTED_ZONE_LOCKED
  ; "ENABLE_FAILED", ENABLE_FAILED
  ; "ENABLING", ENABLING
  ]

let t_to_str =
  [ DISABLING_HOSTED_ZONE_LOCKED, "DISABLING_HOSTED_ZONE_LOCKED"
  ; DISABLED, "DISABLED"
  ; DISABLE_FAILED, "DISABLE_FAILED"
  ; DISABLING, "DISABLING"
  ; ENABLED, "ENABLED"
  ; ENABLING_HOSTED_ZONE_LOCKED, "ENABLING_HOSTED_ZONE_LOCKED"
  ; ENABLE_FAILED, "ENABLE_FAILED"
  ; ENABLING, "ENABLING"
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
