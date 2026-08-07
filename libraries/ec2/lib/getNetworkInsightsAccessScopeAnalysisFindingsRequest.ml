open Aws.BaseTypes

type t =
  { network_insights_access_scope_analysis_id : String.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; dry_run : Boolean.t option
  }

let make ~network_insights_access_scope_analysis_id ?max_results ?next_token ?dry_run () =
  { network_insights_access_scope_analysis_id; max_results; next_token; dry_run }

let parse xml =
  Some
    { network_insights_access_scope_analysis_id =
        Aws.Xml.required
          "NetworkInsightsAccessScopeAnalysisId"
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkInsightsAccessScopeAnalysisId" xml)
             String.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInsightsAccessScopeAnalysisId"
              , String.to_query v.network_insights_access_scope_analysis_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some
           ( "NetworkInsightsAccessScopeAnalysisId"
           , String.to_json v.network_insights_access_scope_analysis_id )
       ])

let of_json j =
  { network_insights_access_scope_analysis_id =
      String.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "NetworkInsightsAccessScopeAnalysisId"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
