open Aws.BaseTypes

type t =
  { network_insights_access_scope_analysis_ids :
      NetworkInsightsAccessScopeAnalysisIdList.t
  ; network_insights_access_scope_id : String.t option
  ; analysis_start_time_begin : DateTime.t option
  ; analysis_start_time_end : DateTime.t option
  ; filters : FilterList.t
  ; max_results : Integer.t option
  ; dry_run : Boolean.t option
  ; next_token : String.t option
  }

let make
    ?(network_insights_access_scope_analysis_ids = [])
    ?network_insights_access_scope_id
    ?analysis_start_time_begin
    ?analysis_start_time_end
    ?(filters = [])
    ?max_results
    ?dry_run
    ?next_token
    () =
  { network_insights_access_scope_analysis_ids
  ; network_insights_access_scope_id
  ; analysis_start_time_begin
  ; analysis_start_time_end
  ; filters
  ; max_results
  ; dry_run
  ; next_token
  }

let parse xml =
  Some
    { network_insights_access_scope_analysis_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkInsightsAccessScopeAnalysisId" xml)
             NetworkInsightsAccessScopeAnalysisIdList.parse)
    ; network_insights_access_scope_id =
        Aws.Util.option_bind
          (Aws.Xml.member "NetworkInsightsAccessScopeId" xml)
          String.parse
    ; analysis_start_time_begin =
        Aws.Util.option_bind (Aws.Xml.member "AnalysisStartTimeBegin" xml) DateTime.parse
    ; analysis_start_time_end =
        Aws.Util.option_bind (Aws.Xml.member "AnalysisStartTimeEnd" xml) DateTime.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.analysis_start_time_end (fun f ->
             Aws.Query.Pair ("AnalysisStartTimeEnd", DateTime.to_query f))
       ; Aws.Util.option_map v.analysis_start_time_begin (fun f ->
             Aws.Query.Pair ("AnalysisStartTimeBegin", DateTime.to_query f))
       ; Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInsightsAccessScopeAnalysisId"
              , NetworkInsightsAccessScopeAnalysisIdList.to_query
                  v.network_insights_access_scope_analysis_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.analysis_start_time_end (fun f ->
             "AnalysisStartTimeEnd", DateTime.to_json f)
       ; Aws.Util.option_map v.analysis_start_time_begin (fun f ->
             "AnalysisStartTimeBegin", DateTime.to_json f)
       ; Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             "NetworkInsightsAccessScopeId", String.to_json f)
       ; Some
           ( "NetworkInsightsAccessScopeAnalysisId"
           , NetworkInsightsAccessScopeAnalysisIdList.to_json
               v.network_insights_access_scope_analysis_ids )
       ])

let of_json j =
  { network_insights_access_scope_analysis_ids =
      NetworkInsightsAccessScopeAnalysisIdList.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "NetworkInsightsAccessScopeAnalysisId"))
  ; network_insights_access_scope_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "NetworkInsightsAccessScopeId")
        String.of_json
  ; analysis_start_time_begin =
      Aws.Util.option_map (Aws.Json.lookup j "AnalysisStartTimeBegin") DateTime.of_json
  ; analysis_start_time_end =
      Aws.Util.option_map (Aws.Json.lookup j "AnalysisStartTimeEnd") DateTime.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
