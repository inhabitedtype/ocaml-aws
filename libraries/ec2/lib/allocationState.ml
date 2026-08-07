open Aws.BaseTypes

type t =
  | Available
  | Under_assessment
  | Permanent_failure
  | Released
  | Released_permanent_failure
  | Pending

let str_to_t =
  [ "pending", Pending
  ; "released-permanent-failure", Released_permanent_failure
  ; "released", Released
  ; "permanent-failure", Permanent_failure
  ; "under-assessment", Under_assessment
  ; "available", Available
  ]

let t_to_str =
  [ Pending, "pending"
  ; Released_permanent_failure, "released-permanent-failure"
  ; Released, "released"
  ; Permanent_failure, "permanent-failure"
  ; Under_assessment, "under-assessment"
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
