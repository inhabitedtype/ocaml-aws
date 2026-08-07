open Aws.BaseTypes

type t =
  { kms_key_id : String.t option
  ; kinesis_stream_name : String.t option
  ; status : ActivityStreamStatus.t option
  ; mode : ActivityStreamMode.t option
  ; engine_native_audit_fields_included : Boolean.t option
  ; policy_status : ActivityStreamPolicyStatus.t option
  }

let make
    ?kms_key_id
    ?kinesis_stream_name
    ?status
    ?mode
    ?engine_native_audit_fields_included
    ?policy_status
    () =
  { kms_key_id
  ; kinesis_stream_name
  ; status
  ; mode
  ; engine_native_audit_fields_included
  ; policy_status
  }

let parse xml =
  Some
    { kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; kinesis_stream_name =
        Aws.Util.option_bind (Aws.Xml.member "KinesisStreamName" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) ActivityStreamStatus.parse
    ; mode = Aws.Util.option_bind (Aws.Xml.member "Mode" xml) ActivityStreamMode.parse
    ; engine_native_audit_fields_included =
        Aws.Util.option_bind
          (Aws.Xml.member "EngineNativeAuditFieldsIncluded" xml)
          Boolean.parse
    ; policy_status =
        Aws.Util.option_bind
          (Aws.Xml.member "PolicyStatus" xml)
          ActivityStreamPolicyStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.policy_status (fun f ->
             Aws.Query.Pair ("PolicyStatus", ActivityStreamPolicyStatus.to_query f))
       ; Aws.Util.option_map v.engine_native_audit_fields_included (fun f ->
             Aws.Query.Pair ("EngineNativeAuditFieldsIncluded", Boolean.to_query f))
       ; Aws.Util.option_map v.mode (fun f ->
             Aws.Query.Pair ("Mode", ActivityStreamMode.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ActivityStreamStatus.to_query f))
       ; Aws.Util.option_map v.kinesis_stream_name (fun f ->
             Aws.Query.Pair ("KinesisStreamName", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.policy_status (fun f ->
             "PolicyStatus", ActivityStreamPolicyStatus.to_json f)
       ; Aws.Util.option_map v.engine_native_audit_fields_included (fun f ->
             "EngineNativeAuditFieldsIncluded", Boolean.to_json f)
       ; Aws.Util.option_map v.mode (fun f -> "Mode", ActivityStreamMode.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", ActivityStreamStatus.to_json f)
       ; Aws.Util.option_map v.kinesis_stream_name (fun f ->
             "KinesisStreamName", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ])

let of_json j =
  { kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; kinesis_stream_name =
      Aws.Util.option_map (Aws.Json.lookup j "KinesisStreamName") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") ActivityStreamStatus.of_json
  ; mode = Aws.Util.option_map (Aws.Json.lookup j "Mode") ActivityStreamMode.of_json
  ; engine_native_audit_fields_included =
      Aws.Util.option_map
        (Aws.Json.lookup j "EngineNativeAuditFieldsIncluded")
        Boolean.of_json
  ; policy_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "PolicyStatus")
        ActivityStreamPolicyStatus.of_json
  }
