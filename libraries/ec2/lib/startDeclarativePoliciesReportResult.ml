open Aws.BaseTypes

type t = { report_id : String.t option }

let make ?report_id () = { report_id }

let parse xml =
  Some { report_id = Aws.Util.option_bind (Aws.Xml.member "reportId" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.report_id (fun f ->
             Aws.Query.Pair ("ReportId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.report_id (fun f -> "reportId", String.to_json f) ])

let of_json j =
  { report_id = Aws.Util.option_map (Aws.Json.lookup j "reportId") String.of_json }
