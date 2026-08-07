type t = { performance_issue_details : PerformanceIssueDetails.t option }

let make ?performance_issue_details () = { performance_issue_details }

let parse xml =
  Some
    { performance_issue_details =
        Aws.Util.option_bind
          (Aws.Xml.member "PerformanceIssueDetails" xml)
          PerformanceIssueDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.performance_issue_details (fun f ->
             Aws.Query.Pair ("PerformanceIssueDetails", PerformanceIssueDetails.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.performance_issue_details (fun f ->
             "PerformanceIssueDetails", PerformanceIssueDetails.to_json f)
       ])

let of_json j =
  { performance_issue_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "PerformanceIssueDetails")
        PerformanceIssueDetails.of_json
  }
