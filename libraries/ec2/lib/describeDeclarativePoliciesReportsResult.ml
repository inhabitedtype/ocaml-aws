open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; reports : DeclarativePoliciesReportList.t
  }

let make ?next_token ?(reports = []) () = { next_token; reports }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; reports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "reportSet" xml)
             DeclarativePoliciesReportList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ReportSet", DeclarativePoliciesReportList.to_query v.reports))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("reportSet", DeclarativePoliciesReportList.to_json v.reports)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; reports =
      DeclarativePoliciesReportList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "reportSet"))
  }
