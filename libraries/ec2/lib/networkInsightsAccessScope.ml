open Aws.BaseTypes

type t =
  { network_insights_access_scope_id : String.t option
  ; network_insights_access_scope_arn : String.t option
  ; created_date : DateTime.t option
  ; updated_date : DateTime.t option
  ; tags : TagList.t
  }

let make
    ?network_insights_access_scope_id
    ?network_insights_access_scope_arn
    ?created_date
    ?updated_date
    ?(tags = [])
    () =
  { network_insights_access_scope_id
  ; network_insights_access_scope_arn
  ; created_date
  ; updated_date
  ; tags
  }

let parse xml =
  Some
    { network_insights_access_scope_id =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeId" xml)
          String.parse
    ; network_insights_access_scope_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAccessScopeArn" xml)
          String.parse
    ; created_date =
        Aws.Util.option_bind (Aws.Xml.member "createdDate" xml) DateTime.parse
    ; updated_date =
        Aws.Util.option_bind (Aws.Xml.member "updatedDate" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.updated_date (fun f ->
             Aws.Query.Pair ("UpdatedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.created_date (fun f ->
             Aws.Query.Pair ("CreatedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.network_insights_access_scope_arn (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeArn", String.to_query f))
       ; Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsAccessScopeId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.updated_date (fun f -> "updatedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.created_date (fun f -> "createdDate", DateTime.to_json f)
       ; Aws.Util.option_map v.network_insights_access_scope_arn (fun f ->
             "networkInsightsAccessScopeArn", String.to_json f)
       ; Aws.Util.option_map v.network_insights_access_scope_id (fun f ->
             "networkInsightsAccessScopeId", String.to_json f)
       ])

let of_json j =
  { network_insights_access_scope_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeId")
        String.of_json
  ; network_insights_access_scope_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAccessScopeArn")
        String.of_json
  ; created_date = Aws.Util.option_map (Aws.Json.lookup j "createdDate") DateTime.of_json
  ; updated_date = Aws.Util.option_map (Aws.Json.lookup j "updatedDate") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
