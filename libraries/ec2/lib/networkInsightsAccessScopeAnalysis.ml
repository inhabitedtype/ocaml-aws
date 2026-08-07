open Aws.BaseTypes

type t =
  { network_insights_access_scope_analysis_id : String.t option
  ; network_insights_access_scope_analysis_arn : String.t option
  ; network_insights_access_scope_id : String.t option
  ; status : AnalysisStatus.t option
  ; status_message : String.t option
  ; warning_message : String.t option
  ; start_date : DateTime.t option
  ; end_date : DateTime.t option
  ; findings_found : FindingsFound.t option
  ; analyzed_eni_count : Integer.t option
  ; tags : TagList.t
  }

let make
    ?network_insights_access_scope_analysis_id
    ?network_insights_access_scope_analysis_arn
    ?network_insights_access_scope_id
    ?status
    ?status_message
    ?warning_message
    ?start_date
    ?end_date
    ?findings_found
    ?analyzed_eni_count
    ?(tags = [])
    () =
  { network_insights_access_scope_analysis_id
  ; network_insights_access_scope_analysis_arn
  ; network_insights_access_scope_id
  ; status
  ; status_message
  ; warning_message
  ; start_date
  ; end_date
  ; findings_found
  ; analyzed_eni_count
  ; tags
  }

let parse xml =
  Some
    { network_insights_access_scope_analysis_id =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeAnalysisId" xml)
          String.parse
    ; network_insights_access_scope_analysis_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeAnalysisArn" xml)
          String.parse
    ; network_insights_access_scope_id =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeId" xml)
          String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) AnalysisStatus.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; warning_message =
        Aws.Util.option_bind (Aws.Xml.member "warningMessage" xml) String.parse
    ; start_date = Aws.Util.option_bind (Aws.Xml.member "startDate" xml) DateTime.parse
    ; end_date = Aws.Util.option_bind (Aws.Xml.member "endDate" xml) DateTime.parse
    ; findings_found =
        Aws.Util.option_bind (Aws.Xml.member "findingsFound" xml) FindingsFound.parse
    ; analyzed_eni_count =
        Aws.Util.option_bind (Aws.Xml.member "analyzedEniCount" xml) Integer.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.analyzed_eni_count (fun f ->
             Aws.Query.Pair ("AnalyzedEniCount", Integer.to_query f))
       ; Aws.Util.option_map v.findings_found (fun f ->
             Aws.Query.Pair ("FindingsFound", FindingsFound.to_query f))
       ; Aws.Util.option_map v.end_date (fun f ->
             Aws.Query.Pair ("EndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.start_date (fun f ->
             Aws.Query.Pair ("StartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.warning_message (fun f ->
             Aws.Query.Pair ("WarningMessage", String.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", AnalysisStatus.to_query f))
       ; Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeId", String.to_query f))
       ; Aws.Util.option_map v.network_insights_access_scope_analysis_arn (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeAnalysisArn", String.to_query f))
       ; Aws.Util.option_map v.network_insights_access_scope_analysis_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeAnalysisId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.analyzed_eni_count (fun f ->
             "analyzedEniCount", Integer.to_json f)
       ; Aws.Util.option_map v.findings_found (fun f ->
             "findingsFound", FindingsFound.to_json f)
       ; Aws.Util.option_map v.end_date (fun f -> "endDate", DateTime.to_json f)
       ; Aws.Util.option_map v.start_date (fun f -> "startDate", DateTime.to_json f)
       ; Aws.Util.option_map v.warning_message (fun f ->
             "warningMessage", String.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", AnalysisStatus.to_json f)
       ; Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             "networkInsightsAccessScopeId", String.to_json f)
       ; Aws.Util.option_map v.network_insights_access_scope_analysis_arn (fun f ->
             "networkInsightsAccessScopeAnalysisArn", String.to_json f)
       ; Aws.Util.option_map v.network_insights_access_scope_analysis_id (fun f ->
             "networkInsightsAccessScopeAnalysisId", String.to_json f)
       ])

let of_json j =
  { network_insights_access_scope_analysis_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeAnalysisId")
        String.of_json
  ; network_insights_access_scope_analysis_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeAnalysisArn")
        String.of_json
  ; network_insights_access_scope_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeId")
        String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") AnalysisStatus.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; warning_message =
      Aws.Util.option_map (Aws.Json.lookup j "warningMessage") String.of_json
  ; start_date = Aws.Util.option_map (Aws.Json.lookup j "startDate") DateTime.of_json
  ; end_date = Aws.Util.option_map (Aws.Json.lookup j "endDate") DateTime.of_json
  ; findings_found =
      Aws.Util.option_map (Aws.Json.lookup j "findingsFound") FindingsFound.of_json
  ; analyzed_eni_count =
      Aws.Util.option_map (Aws.Json.lookup j "analyzedEniCount") Integer.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
