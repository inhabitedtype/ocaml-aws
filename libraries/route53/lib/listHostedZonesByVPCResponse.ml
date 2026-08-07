open Aws.BaseTypes

type t =
  { hosted_zone_summaries : HostedZoneSummaries.t
  ; max_items : String.t
  ; next_token : String.t option
  }

let make ~hosted_zone_summaries ~max_items ?next_token () =
  { hosted_zone_summaries; max_items; next_token }

let parse xml =
  Some
    { hosted_zone_summaries =
        Aws.Xml.required
          "HostedZoneSummaries"
          (Aws.Util.option_bind
             (Aws.Xml.member "HostedZoneSummaries" xml)
             HostedZoneSummaries.parse)
    ; max_items =
        Aws.Xml.required
          "MaxItems"
          (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
       ; Some
           (Aws.Query.Pair
              ( "HostedZoneSummaries.member"
              , HostedZoneSummaries.to_query v.hosted_zone_summaries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("MaxItems", String.to_json v.max_items)
       ; Some ("HostedZoneSummaries", HostedZoneSummaries.to_json v.hosted_zone_summaries)
       ])

let of_json j =
  { hosted_zone_summaries =
      HostedZoneSummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneSummaries"))
  ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
