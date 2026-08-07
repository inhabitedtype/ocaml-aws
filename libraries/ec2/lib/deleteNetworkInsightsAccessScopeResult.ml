open Aws.BaseTypes

type t = { network_insights_access_scope_id : String.t option }

let make ?network_insights_access_scope_id () = { network_insights_access_scope_id }

let parse xml =
  Some
    { network_insights_access_scope_id =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeId" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             "networkInsightsAccessScopeId", String.to_json f)
       ])

let of_json j =
  { network_insights_access_scope_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeId")
        String.of_json
  }
