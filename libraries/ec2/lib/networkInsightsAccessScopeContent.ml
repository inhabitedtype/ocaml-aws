open Aws.BaseTypes

type t =
  { network_insights_access_scope_id : String.t option
  ; match_paths : AccessScopePathList.t
  ; exclude_paths : AccessScopePathList.t
  }

let make ?network_insights_access_scope_id ?(match_paths = []) ?(exclude_paths = []) () =
  { network_insights_access_scope_id; match_paths; exclude_paths }

let parse xml =
  Some
    { network_insights_access_scope_id =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeId" xml)
          String.parse
    ; match_paths =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "matchPathSet" xml)
             AccessScopePathList.parse)
    ; exclude_paths =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "excludePathSet" xml)
             AccessScopePathList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ExcludePathSet", AccessScopePathList.to_query v.exclude_paths))
       ; Some
           (Aws.Query.Pair ("MatchPathSet", AccessScopePathList.to_query v.match_paths))
       ; Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("excludePathSet", AccessScopePathList.to_json v.exclude_paths)
       ; Some ("matchPathSet", AccessScopePathList.to_json v.match_paths)
       ; Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             "networkInsightsAccessScopeId", String.to_json f)
       ])

let of_json j =
  { network_insights_access_scope_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeId")
        String.of_json
  ; match_paths =
      AccessScopePathList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "matchPathSet"))
  ; exclude_paths =
      AccessScopePathList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "excludePathSet"))
  }
