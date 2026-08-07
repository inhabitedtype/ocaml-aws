open Aws.BaseTypes

type t =
  { source_arn : String.t option
  ; target_arn : String.t option
  ; integration_name : String.t option
  ; integration_arn : String.t option
  ; k_m_s_key_id : String.t option
  ; additional_encryption_context : EncryptionContextMap.t option
  ; status : IntegrationStatus.t option
  ; tags : TagList.t
  ; data_filter : String.t option
  ; description : String.t option
  ; create_time : DateTime.t option
  ; errors : IntegrationErrorList.t
  }

let make
    ?source_arn
    ?target_arn
    ?integration_name
    ?integration_arn
    ?k_m_s_key_id
    ?additional_encryption_context
    ?status
    ?(tags = [])
    ?data_filter
    ?description
    ?create_time
    ?(errors = [])
    () =
  { source_arn
  ; target_arn
  ; integration_name
  ; integration_arn
  ; k_m_s_key_id
  ; additional_encryption_context
  ; status
  ; tags
  ; data_filter
  ; description
  ; create_time
  ; errors
  }

let parse xml =
  Some
    { source_arn = Aws.Util.option_bind (Aws.Xml.member "SourceArn" xml) String.parse
    ; target_arn = Aws.Util.option_bind (Aws.Xml.member "TargetArn" xml) String.parse
    ; integration_name =
        Aws.Util.option_bind (Aws.Xml.member "IntegrationName" xml) String.parse
    ; integration_arn =
        Aws.Util.option_bind (Aws.Xml.member "IntegrationArn" xml) String.parse
    ; k_m_s_key_id = Aws.Util.option_bind (Aws.Xml.member "KMSKeyId" xml) String.parse
    ; additional_encryption_context =
        Aws.Util.option_bind
          (Aws.Xml.member "AdditionalEncryptionContext" xml)
          EncryptionContextMap.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) IntegrationStatus.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; data_filter = Aws.Util.option_bind (Aws.Xml.member "DataFilter" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "CreateTime" xml) DateTime.parse
    ; errors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Errors" xml) IntegrationErrorList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Errors.member", IntegrationErrorList.to_query v.errors))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.data_filter (fun f ->
             Aws.Query.Pair ("DataFilter", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", IntegrationStatus.to_query f))
       ; Aws.Util.option_map v.additional_encryption_context (fun f ->
             Aws.Query.Pair
               ("AdditionalEncryptionContext", EncryptionContextMap.to_query f))
       ; Aws.Util.option_map v.k_m_s_key_id (fun f ->
             Aws.Query.Pair ("KMSKeyId", String.to_query f))
       ; Aws.Util.option_map v.integration_arn (fun f ->
             Aws.Query.Pair ("IntegrationArn", String.to_query f))
       ; Aws.Util.option_map v.integration_name (fun f ->
             Aws.Query.Pair ("IntegrationName", String.to_query f))
       ; Aws.Util.option_map v.target_arn (fun f ->
             Aws.Query.Pair ("TargetArn", String.to_query f))
       ; Aws.Util.option_map v.source_arn (fun f ->
             Aws.Query.Pair ("SourceArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Errors", IntegrationErrorList.to_json v.errors)
       ; Aws.Util.option_map v.create_time (fun f -> "CreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.data_filter (fun f -> "DataFilter", String.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.status (fun f -> "Status", IntegrationStatus.to_json f)
       ; Aws.Util.option_map v.additional_encryption_context (fun f ->
             "AdditionalEncryptionContext", EncryptionContextMap.to_json f)
       ; Aws.Util.option_map v.k_m_s_key_id (fun f -> "KMSKeyId", String.to_json f)
       ; Aws.Util.option_map v.integration_arn (fun f ->
             "IntegrationArn", String.to_json f)
       ; Aws.Util.option_map v.integration_name (fun f ->
             "IntegrationName", String.to_json f)
       ; Aws.Util.option_map v.target_arn (fun f -> "TargetArn", String.to_json f)
       ; Aws.Util.option_map v.source_arn (fun f -> "SourceArn", String.to_json f)
       ])

let of_json j =
  { source_arn = Aws.Util.option_map (Aws.Json.lookup j "SourceArn") String.of_json
  ; target_arn = Aws.Util.option_map (Aws.Json.lookup j "TargetArn") String.of_json
  ; integration_name =
      Aws.Util.option_map (Aws.Json.lookup j "IntegrationName") String.of_json
  ; integration_arn =
      Aws.Util.option_map (Aws.Json.lookup j "IntegrationArn") String.of_json
  ; k_m_s_key_id = Aws.Util.option_map (Aws.Json.lookup j "KMSKeyId") String.of_json
  ; additional_encryption_context =
      Aws.Util.option_map
        (Aws.Json.lookup j "AdditionalEncryptionContext")
        EncryptionContextMap.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") IntegrationStatus.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; data_filter = Aws.Util.option_map (Aws.Json.lookup j "DataFilter") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "CreateTime") DateTime.of_json
  ; errors =
      IntegrationErrorList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Errors"))
  }
