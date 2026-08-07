open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; flow_log_ids : FlowLogIdList.t
  }

let make ?dry_run ~flow_log_ids () = { dry_run; flow_log_ids }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; flow_log_ids =
        Aws.Xml.required
          "FlowLogId"
          (Aws.Util.option_bind (Aws.Xml.member "FlowLogId" xml) FlowLogIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("FlowLogId", FlowLogIdList.to_query v.flow_log_ids))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("FlowLogId", FlowLogIdList.to_json v.flow_log_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; flow_log_ids =
      FlowLogIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FlowLogId"))
  }
