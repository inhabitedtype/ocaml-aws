type t =
  { network_insights_access_scope_content : NetworkInsightsAccessScopeContent.t option }

let make ?network_insights_access_scope_content () =
  { network_insights_access_scope_content }

let parse xml =
  Some
    { network_insights_access_scope_content =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeContent" xml)
          NetworkInsightsAccessScopeContent.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_access_scope_content (fun f ->
             Aws.Query.Pair
               ( "NetworkInsightsAccessScopeContent"
               , NetworkInsightsAccessScopeContent.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_access_scope_content (fun f ->
             ( "networkInsightsAccessScopeContent"
             , NetworkInsightsAccessScopeContent.to_json f ))
       ])

let of_json j =
  { network_insights_access_scope_content =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeContent")
        NetworkInsightsAccessScopeContent.of_json
  }
