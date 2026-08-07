open Aws.BaseTypes

type t =
  { network_insights_access_scope_analysis_id : String.t option
  ; analysis_status : AnalysisStatus.t option
  ; analysis_findings : AccessScopeAnalysisFindingList.t
  ; next_token : String.t option
  }

let make
    ?network_insights_access_scope_analysis_id
    ?analysis_status
    ?(analysis_findings = [])
    ?next_token
    () =
  { network_insights_access_scope_analysis_id
  ; analysis_status
  ; analysis_findings
  ; next_token
  }

let parse xml =
  Some
    { network_insights_access_scope_analysis_id =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeAnalysisId" xml)
          String.parse
    ; analysis_status =
        Aws.Util.option_bind (Aws.Xml.member "analysisStatus" xml) AnalysisStatus.parse
    ; analysis_findings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "analysisFindingSet" xml)
             AccessScopeAnalysisFindingList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AnalysisFindingSet"
              , AccessScopeAnalysisFindingList.to_query v.analysis_findings ))
       ; Aws.Util.option_map v.analysis_status (fun f ->
             Aws.Query.Pair ("AnalysisStatus", AnalysisStatus.to_query f))
       ; Aws.Util.option_map v.network_insights_access_scope_analysis_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeAnalysisId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "analysisFindingSet"
           , AccessScopeAnalysisFindingList.to_json v.analysis_findings )
       ; Aws.Util.option_map v.analysis_status (fun f ->
             "analysisStatus", AnalysisStatus.to_json f)
       ; Aws.Util.option_map v.network_insights_access_scope_analysis_id (fun f ->
             "networkInsightsAccessScopeAnalysisId", String.to_json f)
       ])

let of_json j =
  { network_insights_access_scope_analysis_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeAnalysisId")
        String.of_json
  ; analysis_status =
      Aws.Util.option_map (Aws.Json.lookup j "analysisStatus") AnalysisStatus.of_json
  ; analysis_findings =
      AccessScopeAnalysisFindingList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "analysisFindingSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
