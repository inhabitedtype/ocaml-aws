open Aws.BaseTypes

type t =
  { network_insights_access_scope_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~network_insights_access_scope_id ?dry_run () =
  { network_insights_access_scope_id; dry_run }

let parse xml =
  Some
    { network_insights_access_scope_id =
        Aws.Xml.required
          "NetworkInsightsAccessScopeId"
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkInsightsAccessScopeId" xml)
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
              ( "NetworkInsightsAccessScopeId"
              , String.to_query v.network_insights_access_scope_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "NetworkInsightsAccessScopeId"
           , String.to_json v.network_insights_access_scope_id )
       ])

let of_json j =
  { network_insights_access_scope_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInsightsAccessScopeId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
