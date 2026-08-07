open Aws.BaseTypes

type t =
  { network_insights_analysis_ids : NetworkInsightsAnalysisIdList.t
  ; network_insights_path_id : String.t option
  ; analysis_start_time : DateTime.t option
  ; analysis_end_time : DateTime.t option
  ; filters : FilterList.t
  ; max_results : Integer.t option
  ; dry_run : Boolean.t option
  ; next_token : String.t option
  }

let make
    ?(network_insights_analysis_ids = [])
    ?network_insights_path_id
    ?analysis_start_time
    ?analysis_end_time
    ?(filters = [])
    ?max_results
    ?dry_run
    ?next_token
    () =
  { network_insights_analysis_ids
  ; network_insights_path_id
  ; analysis_start_time
  ; analysis_end_time
  ; filters
  ; max_results
  ; dry_run
  ; next_token
  }

let parse xml =
  Some
    { network_insights_analysis_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkInsightsAnalysisId" xml)
             NetworkInsightsAnalysisIdList.parse)
    ; network_insights_path_id =
        Aws.Util.option_bind (Aws.Xml.member "NetworkInsightsPathId" xml) String.parse
    ; analysis_start_time =
        Aws.Util.option_bind (Aws.Xml.member "AnalysisStartTime" xml) DateTime.parse
    ; analysis_end_time =
        Aws.Util.option_bind (Aws.Xml.member "AnalysisEndTime" xml) DateTime.parse
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
       ; Aws.Util.option_map v.analysis_end_time (fun f ->
             Aws.Query.Pair ("AnalysisEndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.analysis_start_time (fun f ->
             Aws.Query.Pair ("AnalysisStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.network_insights_path_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsPathId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInsightsAnalysisId"
              , NetworkInsightsAnalysisIdList.to_query v.network_insights_analysis_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.analysis_end_time (fun f ->
             "AnalysisEndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.analysis_start_time (fun f ->
             "AnalysisStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.network_insights_path_id (fun f ->
             "NetworkInsightsPathId", String.to_json f)
       ; Some
           ( "NetworkInsightsAnalysisId"
           , NetworkInsightsAnalysisIdList.to_json v.network_insights_analysis_ids )
       ])

let of_json j =
  { network_insights_analysis_ids =
      NetworkInsightsAnalysisIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInsightsAnalysisId"))
  ; network_insights_path_id =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkInsightsPathId") String.of_json
  ; analysis_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "AnalysisStartTime") DateTime.of_json
  ; analysis_end_time =
      Aws.Util.option_map (Aws.Json.lookup j "AnalysisEndTime") DateTime.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
