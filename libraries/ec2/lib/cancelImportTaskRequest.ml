open Aws.BaseTypes

type t =
  { cancel_reason : String.t option
  ; dry_run : Boolean.t option
  ; import_task_id : String.t option
  }

let make ?cancel_reason ?dry_run ?import_task_id () =
  { cancel_reason; dry_run; import_task_id }

let parse xml =
  Some
    { cancel_reason =
        Aws.Util.option_bind (Aws.Xml.member "CancelReason" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; import_task_id =
        Aws.Util.option_bind (Aws.Xml.member "ImportTaskId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.import_task_id (fun f ->
             Aws.Query.Pair ("ImportTaskId", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.cancel_reason (fun f ->
             Aws.Query.Pair ("CancelReason", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.import_task_id (fun f -> "ImportTaskId", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.cancel_reason (fun f -> "CancelReason", String.to_json f)
       ])

let of_json j =
  { cancel_reason = Aws.Util.option_map (Aws.Json.lookup j "CancelReason") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; import_task_id = Aws.Util.option_map (Aws.Json.lookup j "ImportTaskId") String.of_json
  }
