open Aws.BaseTypes

type t =
  | CREATED
  | ENABLED
  | PENDING_DELETION
  | STARTING_INGESTION
  | STOPPING_INGESTION
  | STOPPED_INGESTION

let str_to_t =
  [ "STOPPED_INGESTION", STOPPED_INGESTION
  ; "STOPPING_INGESTION", STOPPING_INGESTION
  ; "STARTING_INGESTION", STARTING_INGESTION
  ; "PENDING_DELETION", PENDING_DELETION
  ; "ENABLED", ENABLED
  ; "CREATED", CREATED
  ]

let t_to_str =
  [ STOPPED_INGESTION, "STOPPED_INGESTION"
  ; STOPPING_INGESTION, "STOPPING_INGESTION"
  ; STARTING_INGESTION, "STARTING_INGESTION"
  ; PENDING_DELETION, "PENDING_DELETION"
  ; ENABLED, "ENABLED"
  ; CREATED, "CREATED"
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
