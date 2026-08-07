open Aws.BaseTypes

type t =
  | Initial_ingestion_in_progress
  | Ingestion_complete
  | Ingestion_failed

let str_to_t =
  [ "ingestion-failed", Ingestion_failed
  ; "ingestion-complete", Ingestion_complete
  ; "initial-ingestion-in-progress", Initial_ingestion_in_progress
  ]

let t_to_str =
  [ Ingestion_failed, "ingestion-failed"
  ; Ingestion_complete, "ingestion-complete"
  ; Initial_ingestion_in_progress, "initial-ingestion-in-progress"
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
