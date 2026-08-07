open Aws.BaseTypes

type t =
  { report_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~report_id ?dry_run () = { report_id; dry_run }

let parse xml =
  Some
    { report_id =
        Aws.Xml.required
          "ReportId"
          (Aws.Util.option_bind (Aws.Xml.member "ReportId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("ReportId", String.to_query v.report_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("ReportId", String.to_json v.report_id)
       ])

let of_json j =
  { report_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReportId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
