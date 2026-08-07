open Aws.BaseTypes

type t =
  { network_insights_access_scopes : NetworkInsightsAccessScopeList.t
  ; next_token : String.t option
  }

let make ?(network_insights_access_scopes = []) ?next_token () =
  { network_insights_access_scopes; next_token }

let parse xml =
  Some
    { network_insights_access_scopes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkInsightsAccessScopeSet" xml)
             NetworkInsightsAccessScopeList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInsightsAccessScopeSet"
              , NetworkInsightsAccessScopeList.to_query v.network_insights_access_scopes
              ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "networkInsightsAccessScopeSet"
           , NetworkInsightsAccessScopeList.to_json v.network_insights_access_scopes )
       ])

let of_json j =
  { network_insights_access_scopes =
      NetworkInsightsAccessScopeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInsightsAccessScopeSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
