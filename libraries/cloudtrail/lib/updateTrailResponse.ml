open Aws.BaseTypes

type t =
  { name : String.t option
  ; s3_bucket_name : String.t option
  ; s3_key_prefix : String.t option
  ; sns_topic_name : String.t option
  ; sns_topic_a_r_n : String.t option
  ; include_global_service_events : Boolean.t option
  ; is_multi_region_trail : Boolean.t option
  ; trail_a_r_n : String.t option
  ; log_file_validation_enabled : Boolean.t option
  ; cloud_watch_logs_log_group_arn : String.t option
  ; cloud_watch_logs_role_arn : String.t option
  ; kms_key_id : String.t option
  ; is_organization_trail : Boolean.t option
  }

let make
    ?name
    ?s3_bucket_name
    ?s3_key_prefix
    ?sns_topic_name
    ?sns_topic_a_r_n
    ?include_global_service_events
    ?is_multi_region_trail
    ?trail_a_r_n
    ?log_file_validation_enabled
    ?cloud_watch_logs_log_group_arn
    ?cloud_watch_logs_role_arn
    ?kms_key_id
    ?is_organization_trail
    () =
  { name
  ; s3_bucket_name
  ; s3_key_prefix
  ; sns_topic_name
  ; sns_topic_a_r_n
  ; include_global_service_events
  ; is_multi_region_trail
  ; trail_a_r_n
  ; log_file_validation_enabled
  ; cloud_watch_logs_log_group_arn
  ; cloud_watch_logs_role_arn
  ; kms_key_id
  ; is_organization_trail
  }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; s3_bucket_name =
        Aws.Util.option_bind (Aws.Xml.member "S3BucketName" xml) String.parse
    ; s3_key_prefix = Aws.Util.option_bind (Aws.Xml.member "S3KeyPrefix" xml) String.parse
    ; sns_topic_name =
        Aws.Util.option_bind (Aws.Xml.member "SnsTopicName" xml) String.parse
    ; sns_topic_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "SnsTopicARN" xml) String.parse
    ; include_global_service_events =
        Aws.Util.option_bind
          (Aws.Xml.member "IncludeGlobalServiceEvents" xml)
          Boolean.parse
    ; is_multi_region_trail =
        Aws.Util.option_bind (Aws.Xml.member "IsMultiRegionTrail" xml) Boolean.parse
    ; trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
    ; log_file_validation_enabled =
        Aws.Util.option_bind (Aws.Xml.member "LogFileValidationEnabled" xml) Boolean.parse
    ; cloud_watch_logs_log_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "CloudWatchLogsLogGroupArn" xml) String.parse
    ; cloud_watch_logs_role_arn =
        Aws.Util.option_bind (Aws.Xml.member "CloudWatchLogsRoleArn" xml) String.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; is_organization_trail =
        Aws.Util.option_bind (Aws.Xml.member "IsOrganizationTrail" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_organization_trail (fun f ->
             Aws.Query.Pair ("IsOrganizationTrail", Boolean.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.cloud_watch_logs_role_arn (fun f ->
             Aws.Query.Pair ("CloudWatchLogsRoleArn", String.to_query f))
       ; Aws.Util.option_map v.cloud_watch_logs_log_group_arn (fun f ->
             Aws.Query.Pair ("CloudWatchLogsLogGroupArn", String.to_query f))
       ; Aws.Util.option_map v.log_file_validation_enabled (fun f ->
             Aws.Query.Pair ("LogFileValidationEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.trail_a_r_n (fun f ->
             Aws.Query.Pair ("TrailARN", String.to_query f))
       ; Aws.Util.option_map v.is_multi_region_trail (fun f ->
             Aws.Query.Pair ("IsMultiRegionTrail", Boolean.to_query f))
       ; Aws.Util.option_map v.include_global_service_events (fun f ->
             Aws.Query.Pair ("IncludeGlobalServiceEvents", Boolean.to_query f))
       ; Aws.Util.option_map v.sns_topic_a_r_n (fun f ->
             Aws.Query.Pair ("SnsTopicARN", String.to_query f))
       ; Aws.Util.option_map v.sns_topic_name (fun f ->
             Aws.Query.Pair ("SnsTopicName", String.to_query f))
       ; Aws.Util.option_map v.s3_key_prefix (fun f ->
             Aws.Query.Pair ("S3KeyPrefix", String.to_query f))
       ; Aws.Util.option_map v.s3_bucket_name (fun f ->
             Aws.Query.Pair ("S3BucketName", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_organization_trail (fun f ->
             "IsOrganizationTrail", Boolean.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.cloud_watch_logs_role_arn (fun f ->
             "CloudWatchLogsRoleArn", String.to_json f)
       ; Aws.Util.option_map v.cloud_watch_logs_log_group_arn (fun f ->
             "CloudWatchLogsLogGroupArn", String.to_json f)
       ; Aws.Util.option_map v.log_file_validation_enabled (fun f ->
             "LogFileValidationEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
       ; Aws.Util.option_map v.is_multi_region_trail (fun f ->
             "IsMultiRegionTrail", Boolean.to_json f)
       ; Aws.Util.option_map v.include_global_service_events (fun f ->
             "IncludeGlobalServiceEvents", Boolean.to_json f)
       ; Aws.Util.option_map v.sns_topic_a_r_n (fun f -> "SnsTopicARN", String.to_json f)
       ; Aws.Util.option_map v.sns_topic_name (fun f -> "SnsTopicName", String.to_json f)
       ; Aws.Util.option_map v.s3_key_prefix (fun f -> "S3KeyPrefix", String.to_json f)
       ; Aws.Util.option_map v.s3_bucket_name (fun f -> "S3BucketName", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; s3_bucket_name = Aws.Util.option_map (Aws.Json.lookup j "S3BucketName") String.of_json
  ; s3_key_prefix = Aws.Util.option_map (Aws.Json.lookup j "S3KeyPrefix") String.of_json
  ; sns_topic_name = Aws.Util.option_map (Aws.Json.lookup j "SnsTopicName") String.of_json
  ; sns_topic_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "SnsTopicARN") String.of_json
  ; include_global_service_events =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeGlobalServiceEvents") Boolean.of_json
  ; is_multi_region_trail =
      Aws.Util.option_map (Aws.Json.lookup j "IsMultiRegionTrail") Boolean.of_json
  ; trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
  ; log_file_validation_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "LogFileValidationEnabled") Boolean.of_json
  ; cloud_watch_logs_log_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "CloudWatchLogsLogGroupArn") String.of_json
  ; cloud_watch_logs_role_arn =
      Aws.Util.option_map (Aws.Json.lookup j "CloudWatchLogsRoleArn") String.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; is_organization_trail =
      Aws.Util.option_map (Aws.Json.lookup j "IsOrganizationTrail") Boolean.of_json
  }
