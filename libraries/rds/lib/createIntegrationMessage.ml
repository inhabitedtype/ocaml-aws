open Aws.BaseTypes

type t =
  { source_arn : String.t
  ; target_arn : String.t
  ; integration_name : String.t
  ; k_m_s_key_id : String.t option
  ; additional_encryption_context : EncryptionContextMap.t option
  ; tags : TagList.t
  ; data_filter : String.t option
  ; description : String.t option
  }

let make
    ~source_arn
    ~target_arn
    ~integration_name
    ?k_m_s_key_id
    ?additional_encryption_context
    ?(tags = [])
    ?data_filter
    ?description
    () =
  { source_arn
  ; target_arn
  ; integration_name
  ; k_m_s_key_id
  ; additional_encryption_context
  ; tags
  ; data_filter
  ; description
  }

let parse xml =
  Some
    { source_arn =
        Aws.Xml.required
          "SourceArn"
          (Aws.Util.option_bind (Aws.Xml.member "SourceArn" xml) String.parse)
    ; target_arn =
        Aws.Xml.required
          "TargetArn"
          (Aws.Util.option_bind (Aws.Xml.member "TargetArn" xml) String.parse)
    ; integration_name =
        Aws.Xml.required
          "IntegrationName"
          (Aws.Util.option_bind (Aws.Xml.member "IntegrationName" xml) String.parse)
    ; k_m_s_key_id = Aws.Util.option_bind (Aws.Xml.member "KMSKeyId" xml) String.parse
    ; additional_encryption_context =
        Aws.Util.option_bind
          (Aws.Xml.member "AdditionalEncryptionContext" xml)
          EncryptionContextMap.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; data_filter = Aws.Util.option_bind (Aws.Xml.member "DataFilter" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.data_filter (fun f ->
             Aws.Query.Pair ("DataFilter", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.additional_encryption_context (fun f ->
             Aws.Query.Pair
               ("AdditionalEncryptionContext", EncryptionContextMap.to_query f))
       ; Aws.Util.option_map v.k_m_s_key_id (fun f ->
             Aws.Query.Pair ("KMSKeyId", String.to_query f))
       ; Some (Aws.Query.Pair ("IntegrationName", String.to_query v.integration_name))
       ; Some (Aws.Query.Pair ("TargetArn", String.to_query v.target_arn))
       ; Some (Aws.Query.Pair ("SourceArn", String.to_query v.source_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.data_filter (fun f -> "DataFilter", String.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.additional_encryption_context (fun f ->
             "AdditionalEncryptionContext", EncryptionContextMap.to_json f)
       ; Aws.Util.option_map v.k_m_s_key_id (fun f -> "KMSKeyId", String.to_json f)
       ; Some ("IntegrationName", String.to_json v.integration_name)
       ; Some ("TargetArn", String.to_json v.target_arn)
       ; Some ("SourceArn", String.to_json v.source_arn)
       ])

let of_json j =
  { source_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceArn"))
  ; target_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetArn"))
  ; integration_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IntegrationName"))
  ; k_m_s_key_id = Aws.Util.option_map (Aws.Json.lookup j "KMSKeyId") String.of_json
  ; additional_encryption_context =
      Aws.Util.option_map
        (Aws.Json.lookup j "AdditionalEncryptionContext")
        EncryptionContextMap.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; data_filter = Aws.Util.option_map (Aws.Json.lookup j "DataFilter") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
