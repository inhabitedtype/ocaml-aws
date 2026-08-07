open Aws.BaseTypes

type t =
  { resource_arn : String.t
  ; mode : ActivityStreamMode.t
  ; kms_key_id : String.t
  ; apply_immediately : Boolean.t option
  ; engine_native_audit_fields_included : Boolean.t option
  }

let make
    ~resource_arn
    ~mode
    ~kms_key_id
    ?apply_immediately
    ?engine_native_audit_fields_included
    () =
  { resource_arn
  ; mode
  ; kms_key_id
  ; apply_immediately
  ; engine_native_audit_fields_included
  }

let parse xml =
  Some
    { resource_arn =
        Aws.Xml.required
          "ResourceArn"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceArn" xml) String.parse)
    ; mode =
        Aws.Xml.required
          "Mode"
          (Aws.Util.option_bind (Aws.Xml.member "Mode" xml) ActivityStreamMode.parse)
    ; kms_key_id =
        Aws.Xml.required
          "KmsKeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse)
    ; apply_immediately =
        Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse
    ; engine_native_audit_fields_included =
        Aws.Util.option_bind
          (Aws.Xml.member "EngineNativeAuditFieldsIncluded" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.engine_native_audit_fields_included (fun f ->
             Aws.Query.Pair ("EngineNativeAuditFieldsIncluded", Boolean.to_query f))
       ; Aws.Util.option_map v.apply_immediately (fun f ->
             Aws.Query.Pair ("ApplyImmediately", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("KmsKeyId", String.to_query v.kms_key_id))
       ; Some (Aws.Query.Pair ("Mode", ActivityStreamMode.to_query v.mode))
       ; Some (Aws.Query.Pair ("ResourceArn", String.to_query v.resource_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.engine_native_audit_fields_included (fun f ->
             "EngineNativeAuditFieldsIncluded", Boolean.to_json f)
       ; Aws.Util.option_map v.apply_immediately (fun f ->
             "ApplyImmediately", Boolean.to_json f)
       ; Some ("KmsKeyId", String.to_json v.kms_key_id)
       ; Some ("Mode", ActivityStreamMode.to_json v.mode)
       ; Some ("ResourceArn", String.to_json v.resource_arn)
       ])

let of_json j =
  { resource_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceArn"))
  ; mode = ActivityStreamMode.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Mode"))
  ; kms_key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KmsKeyId"))
  ; apply_immediately =
      Aws.Util.option_map (Aws.Json.lookup j "ApplyImmediately") Boolean.of_json
  ; engine_native_audit_fields_included =
      Aws.Util.option_map
        (Aws.Json.lookup j "EngineNativeAuditFieldsIncluded")
        Boolean.of_json
  }
