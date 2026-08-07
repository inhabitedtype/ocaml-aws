open Aws.BaseTypes

type t =
  | HTTP
  | HTTPS
  | HTTP_STR_MATCH
  | HTTPS_STR_MATCH
  | TCP
  | CALCULATED
  | CLOUDWATCH_METRIC
  | RECOVERY_CONTROL

let str_to_t =
  [ "RECOVERY_CONTROL", RECOVERY_CONTROL
  ; "CLOUDWATCH_METRIC", CLOUDWATCH_METRIC
  ; "CALCULATED", CALCULATED
  ; "TCP", TCP
  ; "HTTPS_STR_MATCH", HTTPS_STR_MATCH
  ; "HTTP_STR_MATCH", HTTP_STR_MATCH
  ; "HTTPS", HTTPS
  ; "HTTP", HTTP
  ]

let t_to_str =
  [ RECOVERY_CONTROL, "RECOVERY_CONTROL"
  ; CLOUDWATCH_METRIC, "CLOUDWATCH_METRIC"
  ; CALCULATED, "CALCULATED"
  ; TCP, "TCP"
  ; HTTPS_STR_MATCH, "HTTPS_STR_MATCH"
  ; HTTP_STR_MATCH, "HTTP_STR_MATCH"
  ; HTTPS, "HTTPS"
  ; HTTP, "HTTP"
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
