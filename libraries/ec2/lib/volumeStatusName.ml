open Aws.BaseTypes

type t =
  | Io_enabled
  | Io_performance
  | Initialization_state

let str_to_t =
  [ "initialization-state", Initialization_state
  ; "io-performance", Io_performance
  ; "io-enabled", Io_enabled
  ]

let t_to_str =
  [ Initialization_state, "initialization-state"
  ; Io_performance, "io-performance"
  ; Io_enabled, "io-enabled"
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
