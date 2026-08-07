open Aws.BaseTypes

type t = { network_insights_access_scope_analysis_id : String.t option }

let make ?network_insights_access_scope_analysis_id () =
  { network_insights_access_scope_analysis_id }

let parse xml =
  Some
    { network_insights_access_scope_analysis_id =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeAnalysisId" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_access_scope_analysis_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeAnalysisId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_access_scope_analysis_id (fun f ->
             "networkInsightsAccessScopeAnalysisId", String.to_json f)
       ])

let of_json j =
  { network_insights_access_scope_analysis_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeAnalysisId")
        String.of_json
  }
