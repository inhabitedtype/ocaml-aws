open Aws.BaseTypes

type t =
  { role_arn : String.t
  ; role_session_name : String.t
  ; policy_arns : PolicyDescriptorListType.t
  ; policy : String.t option
  ; duration_seconds : Integer.t option
  ; tags : TagListType.t
  ; transitive_tag_keys : TagKeyListType.t
  ; external_id : String.t option
  ; serial_number : String.t option
  ; token_code : String.t option
  ; source_identity : String.t option
  ; provided_contexts : ProvidedContextsListType.t
  }

let make
    ~role_arn
    ~role_session_name
    ?(policy_arns = [])
    ?policy
    ?duration_seconds
    ?(tags = [])
    ?(transitive_tag_keys = [])
    ?external_id
    ?serial_number
    ?token_code
    ?source_identity
    ?(provided_contexts = [])
    () =
  { role_arn
  ; role_session_name
  ; policy_arns
  ; policy
  ; duration_seconds
  ; tags
  ; transitive_tag_keys
  ; external_id
  ; serial_number
  ; token_code
  ; source_identity
  ; provided_contexts
  }

let parse xml =
  Some
    { role_arn =
        Aws.Xml.required
          "RoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse)
    ; role_session_name =
        Aws.Xml.required
          "RoleSessionName"
          (Aws.Util.option_bind (Aws.Xml.member "RoleSessionName" xml) String.parse)
    ; policy_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PolicyArns" xml)
             PolicyDescriptorListType.parse)
    ; policy = Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse
    ; duration_seconds =
        Aws.Util.option_bind (Aws.Xml.member "DurationSeconds" xml) Integer.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagListType.parse)
    ; transitive_tag_keys =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitiveTagKeys" xml)
             TagKeyListType.parse)
    ; external_id = Aws.Util.option_bind (Aws.Xml.member "ExternalId" xml) String.parse
    ; serial_number =
        Aws.Util.option_bind (Aws.Xml.member "SerialNumber" xml) String.parse
    ; token_code = Aws.Util.option_bind (Aws.Xml.member "TokenCode" xml) String.parse
    ; source_identity =
        Aws.Util.option_bind (Aws.Xml.member "SourceIdentity" xml) String.parse
    ; provided_contexts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ProvidedContexts" xml)
             ProvidedContextsListType.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ProvidedContexts.member"
              , ProvidedContextsListType.to_query v.provided_contexts ))
       ; Aws.Util.option_map v.source_identity (fun f ->
             Aws.Query.Pair ("SourceIdentity", String.to_query f))
       ; Aws.Util.option_map v.token_code (fun f ->
             Aws.Query.Pair ("TokenCode", String.to_query f))
       ; Aws.Util.option_map v.serial_number (fun f ->
             Aws.Query.Pair ("SerialNumber", String.to_query f))
       ; Aws.Util.option_map v.external_id (fun f ->
             Aws.Query.Pair ("ExternalId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TransitiveTagKeys.member", TagKeyListType.to_query v.transitive_tag_keys))
       ; Some (Aws.Query.Pair ("Tags.member", TagListType.to_query v.tags))
       ; Aws.Util.option_map v.duration_seconds (fun f ->
             Aws.Query.Pair ("DurationSeconds", Integer.to_query f))
       ; Aws.Util.option_map v.policy (fun f ->
             Aws.Query.Pair ("Policy", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("PolicyArns.member", PolicyDescriptorListType.to_query v.policy_arns))
       ; Some (Aws.Query.Pair ("RoleSessionName", String.to_query v.role_session_name))
       ; Some (Aws.Query.Pair ("RoleArn", String.to_query v.role_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ProvidedContexts", ProvidedContextsListType.to_json v.provided_contexts)
       ; Aws.Util.option_map v.source_identity (fun f ->
             "SourceIdentity", String.to_json f)
       ; Aws.Util.option_map v.token_code (fun f -> "TokenCode", String.to_json f)
       ; Aws.Util.option_map v.serial_number (fun f -> "SerialNumber", String.to_json f)
       ; Aws.Util.option_map v.external_id (fun f -> "ExternalId", String.to_json f)
       ; Some ("TransitiveTagKeys", TagKeyListType.to_json v.transitive_tag_keys)
       ; Some ("Tags", TagListType.to_json v.tags)
       ; Aws.Util.option_map v.duration_seconds (fun f ->
             "DurationSeconds", Integer.to_json f)
       ; Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f)
       ; Some ("PolicyArns", PolicyDescriptorListType.to_json v.policy_arns)
       ; Some ("RoleSessionName", String.to_json v.role_session_name)
       ; Some ("RoleArn", String.to_json v.role_arn)
       ])

let of_json j =
  { role_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleArn"))
  ; role_session_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleSessionName"))
  ; policy_arns =
      PolicyDescriptorListType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyArns"))
  ; policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json
  ; duration_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "DurationSeconds") Integer.of_json
  ; tags = TagListType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; transitive_tag_keys =
      TagKeyListType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitiveTagKeys"))
  ; external_id = Aws.Util.option_map (Aws.Json.lookup j "ExternalId") String.of_json
  ; serial_number = Aws.Util.option_map (Aws.Json.lookup j "SerialNumber") String.of_json
  ; token_code = Aws.Util.option_map (Aws.Json.lookup j "TokenCode") String.of_json
  ; source_identity =
      Aws.Util.option_map (Aws.Json.lookup j "SourceIdentity") String.of_json
  ; provided_contexts =
      ProvidedContextsListType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ProvidedContexts"))
  }
