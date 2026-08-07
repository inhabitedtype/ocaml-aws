open Aws.BaseTypes

type t =
  { network_insights_access_scope_analysis_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~network_insights_access_scope_analysis_id ?dry_run () =
  { network_insights_access_scope_analysis_id; dry_run }

let parse xml =
  Some
    { network_insights_access_scope_analysis_id =
        Aws.Xml.required
          "NetworkInsightsAccessScopeAnalysisId"
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkInsightsAccessScopeAnalysisId" xml)
             String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInsightsAccessScopeAnalysisId"
              , String.to_query v.network_insights_access_scope_analysis_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "NetworkInsightsAccessScopeAnalysisId"
           , String.to_json v.network_insights_access_scope_analysis_id )
       ])

let of_json j =
  { network_insights_access_scope_analysis_id =
      String.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "NetworkInsightsAccessScopeAnalysisId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
