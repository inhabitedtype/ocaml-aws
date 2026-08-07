open Aws.BaseTypes

type t =
  { network_insights_access_scope_analysis_id : String.t option
  ; network_insights_access_scope_id : String.t option
  ; finding_id : String.t option
  ; finding_components : PathComponentList.t
  }

let make
    ?network_insights_access_scope_analysis_id
    ?network_insights_access_scope_id
    ?finding_id
    ?(finding_components = [])
    () =
  { network_insights_access_scope_analysis_id
  ; network_insights_access_scope_id
  ; finding_id
  ; finding_components
  }

let parse xml =
  Some
    { network_insights_access_scope_analysis_id =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeAnalysisId" xml)
          String.parse
    ; network_insights_access_scope_id =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeId" xml)
          String.parse
    ; finding_id = Aws.Util.option_bind (Aws.Xml.member "findingId" xml) String.parse
    ; finding_components =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "findingComponentSet" xml)
             PathComponentList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("FindingComponentSet", PathComponentList.to_query v.finding_components))
       ; Aws.Util.option_map v.finding_id (fun f ->
             Aws.Query.Pair ("FindingId", String.to_query f))
       ; Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeId", String.to_query f))
       ; Aws.Util.option_map v.network_insights_access_scope_analysis_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeAnalysisId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("findingComponentSet", PathComponentList.to_json v.finding_components)
       ; Aws.Util.option_map v.finding_id (fun f -> "findingId", String.to_json f)
       ; Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             "networkInsightsAccessScopeId", String.to_json f)
       ; Aws.Util.option_map v.network_insights_access_scope_analysis_id (fun f ->
             "networkInsightsAccessScopeAnalysisId", String.to_json f)
       ])

let of_json j =
  { network_insights_access_scope_analysis_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeAnalysisId")
        String.of_json
  ; network_insights_access_scope_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeId")
        String.of_json
  ; finding_id = Aws.Util.option_map (Aws.Json.lookup j "findingId") String.of_json
  ; finding_components =
      PathComponentList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "findingComponentSet"))
  }
