open Aws.BaseTypes

type t =
  { resource_scan_summaries : ResourceScanSummaries.t
  ; next_token : String.t option
  }

let make ?(resource_scan_summaries = []) ?next_token () =
  { resource_scan_summaries; next_token }

let parse xml =
  Some
    { resource_scan_summaries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceScanSummaries" xml)
             ResourceScanSummaries.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ResourceScanSummaries.member"
              , ResourceScanSummaries.to_query v.resource_scan_summaries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "ResourceScanSummaries"
           , ResourceScanSummaries.to_json v.resource_scan_summaries )
       ])

let of_json j =
  { resource_scan_summaries =
      ResourceScanSummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceScanSummaries"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
