open Aws.BaseTypes

type t =
  | Json
  | Opentelemetry0_7
  | Opentelemetry1_0

let str_to_t =
  [ "opentelemetry1.0", Opentelemetry1_0
  ; "opentelemetry0.7", Opentelemetry0_7
  ; "json", Json
  ]

let t_to_str =
  [ Opentelemetry1_0, "opentelemetry1.0"
  ; Opentelemetry0_7, "opentelemetry0.7"
  ; Json, "json"
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
