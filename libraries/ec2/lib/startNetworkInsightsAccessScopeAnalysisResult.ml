type t =
  { network_insights_access_scope_analysis : NetworkInsightsAccessScopeAnalysis.t option }

let make ?network_insights_access_scope_analysis () =
  { network_insights_access_scope_analysis }

let parse xml =
  Some
    { network_insights_access_scope_analysis =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeAnalysis" xml)
          NetworkInsightsAccessScopeAnalysis.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_access_scope_analysis (fun f ->
             Aws.Query.Pair
               ( "NetworkInsightsAccessScopeAnalysis"
               , NetworkInsightsAccessScopeAnalysis.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_access_scope_analysis (fun f ->
             ( "networkInsightsAccessScopeAnalysis"
             , NetworkInsightsAccessScopeAnalysis.to_json f ))
       ])

let of_json j =
  { network_insights_access_scope_analysis =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeAnalysis")
        NetworkInsightsAccessScopeAnalysis.of_json
  }
