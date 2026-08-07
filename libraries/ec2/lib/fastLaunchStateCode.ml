open Aws.BaseTypes

type t =
  | Enabling
  | Enabling_failed
  | Enabled
  | Enabled_failed
  | Disabling
  | Disabling_failed

let str_to_t =
  [ "disabling-failed", Disabling_failed
  ; "disabling", Disabling
  ; "enabled-failed", Enabled_failed
  ; "enabled", Enabled
  ; "enabling-failed", Enabling_failed
  ; "enabling", Enabling
  ]

let t_to_str =
  [ Disabling_failed, "disabling-failed"
  ; Disabling, "disabling"
  ; Enabled_failed, "enabled-failed"
  ; Enabled, "enabled"
  ; Enabling_failed, "enabling-failed"
  ; Enabling, "enabling"
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
