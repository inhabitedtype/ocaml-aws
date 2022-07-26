open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

module Trail = struct
  type t =
    { name : String.t option
    ; s3_bucket_name : String.t option
    ; s3_key_prefix : String.t option
    ; sns_topic_name : String.t option
    ; sns_topic_a_r_n : String.t option
    ; include_global_service_events : Boolean.t option
    ; is_multi_region_trail : Boolean.t option
    ; home_region : String.t option
    ; trail_a_r_n : String.t option
    ; log_file_validation_enabled : Boolean.t option
    ; cloud_watch_logs_log_group_arn : String.t option
    ; cloud_watch_logs_role_arn : String.t option
    ; kms_key_id : String.t option
    ; has_custom_event_selectors : Boolean.t option
    ; has_insight_selectors : Boolean.t option
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
      ?home_region
      ?trail_a_r_n
      ?log_file_validation_enabled
      ?cloud_watch_logs_log_group_arn
      ?cloud_watch_logs_role_arn
      ?kms_key_id
      ?has_custom_event_selectors
      ?has_insight_selectors
      ?is_organization_trail
      () =
    { name
    ; s3_bucket_name
    ; s3_key_prefix
    ; sns_topic_name
    ; sns_topic_a_r_n
    ; include_global_service_events
    ; is_multi_region_trail
    ; home_region
    ; trail_a_r_n
    ; log_file_validation_enabled
    ; cloud_watch_logs_log_group_arn
    ; cloud_watch_logs_role_arn
    ; kms_key_id
    ; has_custom_event_selectors
    ; has_insight_selectors
    ; is_organization_trail
    }

  let parse xml =
    Some
      { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; s3_bucket_name =
          Aws.Util.option_bind (Aws.Xml.member "S3BucketName" xml) String.parse
      ; s3_key_prefix =
          Aws.Util.option_bind (Aws.Xml.member "S3KeyPrefix" xml) String.parse
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
      ; home_region = Aws.Util.option_bind (Aws.Xml.member "HomeRegion" xml) String.parse
      ; trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
      ; log_file_validation_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "LogFileValidationEnabled" xml)
            Boolean.parse
      ; cloud_watch_logs_log_group_arn =
          Aws.Util.option_bind
            (Aws.Xml.member "CloudWatchLogsLogGroupArn" xml)
            String.parse
      ; cloud_watch_logs_role_arn =
          Aws.Util.option_bind (Aws.Xml.member "CloudWatchLogsRoleArn" xml) String.parse
      ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
      ; has_custom_event_selectors =
          Aws.Util.option_bind
            (Aws.Xml.member "HasCustomEventSelectors" xml)
            Boolean.parse
      ; has_insight_selectors =
          Aws.Util.option_bind (Aws.Xml.member "HasInsightSelectors" xml) Boolean.parse
      ; is_organization_trail =
          Aws.Util.option_bind (Aws.Xml.member "IsOrganizationTrail" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.is_organization_trail (fun f ->
               Aws.Query.Pair ("IsOrganizationTrail", Boolean.to_query f))
         ; Aws.Util.option_map v.has_insight_selectors (fun f ->
               Aws.Query.Pair ("HasInsightSelectors", Boolean.to_query f))
         ; Aws.Util.option_map v.has_custom_event_selectors (fun f ->
               Aws.Query.Pair ("HasCustomEventSelectors", Boolean.to_query f))
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
         ; Aws.Util.option_map v.home_region (fun f ->
               Aws.Query.Pair ("HomeRegion", String.to_query f))
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
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.is_organization_trail (fun f ->
               "IsOrganizationTrail", Boolean.to_json f)
         ; Aws.Util.option_map v.has_insight_selectors (fun f ->
               "HasInsightSelectors", Boolean.to_json f)
         ; Aws.Util.option_map v.has_custom_event_selectors (fun f ->
               "HasCustomEventSelectors", Boolean.to_json f)
         ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
         ; Aws.Util.option_map v.cloud_watch_logs_role_arn (fun f ->
               "CloudWatchLogsRoleArn", String.to_json f)
         ; Aws.Util.option_map v.cloud_watch_logs_log_group_arn (fun f ->
               "CloudWatchLogsLogGroupArn", String.to_json f)
         ; Aws.Util.option_map v.log_file_validation_enabled (fun f ->
               "LogFileValidationEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
         ; Aws.Util.option_map v.home_region (fun f -> "HomeRegion", String.to_json f)
         ; Aws.Util.option_map v.is_multi_region_trail (fun f ->
               "IsMultiRegionTrail", Boolean.to_json f)
         ; Aws.Util.option_map v.include_global_service_events (fun f ->
               "IncludeGlobalServiceEvents", Boolean.to_json f)
         ; Aws.Util.option_map v.sns_topic_a_r_n (fun f ->
               "SnsTopicARN", String.to_json f)
         ; Aws.Util.option_map v.sns_topic_name (fun f ->
               "SnsTopicName", String.to_json f)
         ; Aws.Util.option_map v.s3_key_prefix (fun f -> "S3KeyPrefix", String.to_json f)
         ; Aws.Util.option_map v.s3_bucket_name (fun f ->
               "S3BucketName", String.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ])

  let of_json j =
    { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; s3_bucket_name =
        Aws.Util.option_map (Aws.Json.lookup j "S3BucketName") String.of_json
    ; s3_key_prefix = Aws.Util.option_map (Aws.Json.lookup j "S3KeyPrefix") String.of_json
    ; sns_topic_name =
        Aws.Util.option_map (Aws.Json.lookup j "SnsTopicName") String.of_json
    ; sns_topic_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "SnsTopicARN") String.of_json
    ; include_global_service_events =
        Aws.Util.option_map
          (Aws.Json.lookup j "IncludeGlobalServiceEvents")
          Boolean.of_json
    ; is_multi_region_trail =
        Aws.Util.option_map (Aws.Json.lookup j "IsMultiRegionTrail") Boolean.of_json
    ; home_region = Aws.Util.option_map (Aws.Json.lookup j "HomeRegion") String.of_json
    ; trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
    ; log_file_validation_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "LogFileValidationEnabled") Boolean.of_json
    ; cloud_watch_logs_log_group_arn =
        Aws.Util.option_map (Aws.Json.lookup j "CloudWatchLogsLogGroupArn") String.of_json
    ; cloud_watch_logs_role_arn =
        Aws.Util.option_map (Aws.Json.lookup j "CloudWatchLogsRoleArn") String.of_json
    ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
    ; has_custom_event_selectors =
        Aws.Util.option_map (Aws.Json.lookup j "HasCustomEventSelectors") Boolean.of_json
    ; has_insight_selectors =
        Aws.Util.option_map (Aws.Json.lookup j "HasInsightSelectors") Boolean.of_json
    ; is_organization_trail =
        Aws.Util.option_map (Aws.Json.lookup j "IsOrganizationTrail") Boolean.of_json
    }
end

module TrailList = struct
  type t = Trail.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Trail.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Trail.to_query v

  let to_json v = `List (List.map Trail.to_json v)

  let of_json j = Aws.Json.to_list Trail.of_json j
end

module DescribeTrailsResponse = struct
  type t = { trail_list : TrailList.t }

  let make ?(trail_list = []) () = { trail_list }

  let parse xml =
    Some
      { trail_list =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "trailList" xml) TrailList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("trailList.member", TrailList.to_query v.trail_list)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("trailList", TrailList.to_json v.trail_list) ])

  let of_json j =
    { trail_list =
        TrailList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "trailList"))
    }
end

module CreateTrailResponse = struct
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
      ; s3_key_prefix =
          Aws.Util.option_bind (Aws.Xml.member "S3KeyPrefix" xml) String.parse
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
          Aws.Util.option_bind
            (Aws.Xml.member "LogFileValidationEnabled" xml)
            Boolean.parse
      ; cloud_watch_logs_log_group_arn =
          Aws.Util.option_bind
            (Aws.Xml.member "CloudWatchLogsLogGroupArn" xml)
            String.parse
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
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
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
         ; Aws.Util.option_map v.sns_topic_a_r_n (fun f ->
               "SnsTopicARN", String.to_json f)
         ; Aws.Util.option_map v.sns_topic_name (fun f ->
               "SnsTopicName", String.to_json f)
         ; Aws.Util.option_map v.s3_key_prefix (fun f -> "S3KeyPrefix", String.to_json f)
         ; Aws.Util.option_map v.s3_bucket_name (fun f ->
               "S3BucketName", String.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ])

  let of_json j =
    { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; s3_bucket_name =
        Aws.Util.option_map (Aws.Json.lookup j "S3BucketName") String.of_json
    ; s3_key_prefix = Aws.Util.option_map (Aws.Json.lookup j "S3KeyPrefix") String.of_json
    ; sns_topic_name =
        Aws.Util.option_map (Aws.Json.lookup j "SnsTopicName") String.of_json
    ; sns_topic_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "SnsTopicARN") String.of_json
    ; include_global_service_events =
        Aws.Util.option_map
          (Aws.Json.lookup j "IncludeGlobalServiceEvents")
          Boolean.of_json
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
end

module Tag = struct
  type t =
    { key : String.t
    ; value : String.t option
    }

  let make ~key ?value () = { key; value }

  let parse xml =
    Some
      { key =
          Aws.Xml.required
            "Key"
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse)
      ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", String.to_query f))
         ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
         ; Some ("Key", String.to_json v.key)
         ])

  let of_json j =
    { key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key"))
    ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
    }
end

module TagsList = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map Tag.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Aws.Json.to_list Tag.of_json j
end

module ResourceTag = struct
  type t =
    { resource_id : String.t option
    ; tags_list : TagsList.t
    }

  let make ?resource_id ?(tags_list = []) () = { resource_id; tags_list }

  let parse xml =
    Some
      { resource_id = Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse
      ; tags_list =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "TagsList" xml) TagsList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TagsList.member", TagsList.to_query v.tags_list))
         ; Aws.Util.option_map v.resource_id (fun f ->
               Aws.Query.Pair ("ResourceId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TagsList", TagsList.to_json v.tags_list)
         ; Aws.Util.option_map v.resource_id (fun f -> "ResourceId", String.to_json f)
         ])

  let of_json j =
    { resource_id = Aws.Util.option_map (Aws.Json.lookup j "ResourceId") String.of_json
    ; tags_list = TagsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagsList"))
    }
end

module LookupAttributeKey = struct
  type t =
    | EventId
    | EventName
    | ReadOnly
    | Username
    | ResourceType
    | ResourceName
    | EventSource
    | AccessKeyId

  let str_to_t =
    [ "AccessKeyId", AccessKeyId
    ; "EventSource", EventSource
    ; "ResourceName", ResourceName
    ; "ResourceType", ResourceType
    ; "Username", Username
    ; "ReadOnly", ReadOnly
    ; "EventName", EventName
    ; "EventId", EventId
    ]

  let t_to_str =
    [ AccessKeyId, "AccessKeyId"
    ; EventSource, "EventSource"
    ; ResourceName, "ResourceName"
    ; ResourceType, "ResourceType"
    ; Username, "Username"
    ; ReadOnly, "ReadOnly"
    ; EventName, "EventName"
    ; EventId, "EventId"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module LookupAttribute = struct
  type t =
    { attribute_key : LookupAttributeKey.t
    ; attribute_value : String.t
    }

  let make ~attribute_key ~attribute_value () = { attribute_key; attribute_value }

  let parse xml =
    Some
      { attribute_key =
          Aws.Xml.required
            "AttributeKey"
            (Aws.Util.option_bind
               (Aws.Xml.member "AttributeKey" xml)
               LookupAttributeKey.parse)
      ; attribute_value =
          Aws.Xml.required
            "AttributeValue"
            (Aws.Util.option_bind (Aws.Xml.member "AttributeValue" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("AttributeValue", String.to_query v.attribute_value))
         ; Some
             (Aws.Query.Pair ("AttributeKey", LookupAttributeKey.to_query v.attribute_key))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AttributeValue", String.to_json v.attribute_value)
         ; Some ("AttributeKey", LookupAttributeKey.to_json v.attribute_key)
         ])

  let of_json j =
    { attribute_key =
        LookupAttributeKey.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeKey"))
    ; attribute_value =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeValue"))
    }
end

module GetTrailRequest = struct
  type t = { name : String.t }

  let make ~name () = { name }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Name", String.to_query v.name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Name", String.to_json v.name) ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name")) }
end

module InvalidSnsTopicNameException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module PublicKey = struct
  type t =
    { value : Blob.t option
    ; validity_start_time : DateTime.t option
    ; validity_end_time : DateTime.t option
    ; fingerprint : String.t option
    }

  let make ?value ?validity_start_time ?validity_end_time ?fingerprint () =
    { value; validity_start_time; validity_end_time; fingerprint }

  let parse xml =
    Some
      { value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) Blob.parse
      ; validity_start_time =
          Aws.Util.option_bind (Aws.Xml.member "ValidityStartTime" xml) DateTime.parse
      ; validity_end_time =
          Aws.Util.option_bind (Aws.Xml.member "ValidityEndTime" xml) DateTime.parse
      ; fingerprint = Aws.Util.option_bind (Aws.Xml.member "Fingerprint" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.fingerprint (fun f ->
               Aws.Query.Pair ("Fingerprint", String.to_query f))
         ; Aws.Util.option_map v.validity_end_time (fun f ->
               Aws.Query.Pair ("ValidityEndTime", DateTime.to_query f))
         ; Aws.Util.option_map v.validity_start_time (fun f ->
               Aws.Query.Pair ("ValidityStartTime", DateTime.to_query f))
         ; Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.fingerprint (fun f -> "Fingerprint", String.to_json f)
         ; Aws.Util.option_map v.validity_end_time (fun f ->
               "ValidityEndTime", DateTime.to_json f)
         ; Aws.Util.option_map v.validity_start_time (fun f ->
               "ValidityStartTime", DateTime.to_json f)
         ; Aws.Util.option_map v.value (fun f -> "Value", Blob.to_json f)
         ])

  let of_json j =
    { value = Aws.Util.option_map (Aws.Json.lookup j "Value") Blob.of_json
    ; validity_start_time =
        Aws.Util.option_map (Aws.Json.lookup j "ValidityStartTime") DateTime.of_json
    ; validity_end_time =
        Aws.Util.option_map (Aws.Json.lookup j "ValidityEndTime") DateTime.of_json
    ; fingerprint = Aws.Util.option_map (Aws.Json.lookup j "Fingerprint") String.of_json
    }
end

module PublicKeyList = struct
  type t = PublicKey.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map PublicKey.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list PublicKey.to_query v

  let to_json v = `List (List.map PublicKey.to_json v)

  let of_json j = Aws.Json.to_list PublicKey.of_json j
end

module ListPublicKeysResponse = struct
  type t =
    { public_key_list : PublicKeyList.t
    ; next_token : String.t option
    }

  let make ?(public_key_list = []) ?next_token () = { public_key_list; next_token }

  let parse xml =
    Some
      { public_key_list =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PublicKeyList" xml)
               PublicKeyList.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("PublicKeyList.member", PublicKeyList.to_query v.public_key_list))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("PublicKeyList", PublicKeyList.to_json v.public_key_list)
         ])

  let of_json j =
    { public_key_list =
        PublicKeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PublicKeyList"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ReadWriteType = struct
  type t =
    | ReadOnly
    | WriteOnly
    | All

  let str_to_t = [ "All", All; "WriteOnly", WriteOnly; "ReadOnly", ReadOnly ]

  let t_to_str = [ All, "All"; WriteOnly, "WriteOnly"; ReadOnly, "ReadOnly" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ExcludeManagementEventSources = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DataResourceValues = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DataResource = struct
  type t =
    { type_ : String.t option
    ; values : DataResourceValues.t
    }

  let make ?type_ ?(values = []) () = { type_; values }

  let parse xml =
    Some
      { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) String.parse
      ; values =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Values" xml) DataResourceValues.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Values.member", DataResourceValues.to_query v.values))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Values", DataResourceValues.to_json v.values)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", String.to_json f)
         ])

  let of_json j =
    { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") String.of_json
    ; values =
        DataResourceValues.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Values"))
    }
end

module DataResources = struct
  type t = DataResource.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map DataResource.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list DataResource.to_query v

  let to_json v = `List (List.map DataResource.to_json v)

  let of_json j = Aws.Json.to_list DataResource.of_json j
end

module EventSelector = struct
  type t =
    { read_write_type : ReadWriteType.t option
    ; include_management_events : Boolean.t option
    ; data_resources : DataResources.t
    ; exclude_management_event_sources : ExcludeManagementEventSources.t
    }

  let make
      ?read_write_type
      ?include_management_events
      ?(data_resources = [])
      ?(exclude_management_event_sources = [])
      () =
    { read_write_type
    ; include_management_events
    ; data_resources
    ; exclude_management_event_sources
    }

  let parse xml =
    Some
      { read_write_type =
          Aws.Util.option_bind (Aws.Xml.member "ReadWriteType" xml) ReadWriteType.parse
      ; include_management_events =
          Aws.Util.option_bind
            (Aws.Xml.member "IncludeManagementEvents" xml)
            Boolean.parse
      ; data_resources =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "DataResources" xml)
               DataResources.parse)
      ; exclude_management_event_sources =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ExcludeManagementEventSources" xml)
               ExcludeManagementEventSources.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ExcludeManagementEventSources.member"
                , ExcludeManagementEventSources.to_query
                    v.exclude_management_event_sources ))
         ; Some
             (Aws.Query.Pair
                ("DataResources.member", DataResources.to_query v.data_resources))
         ; Aws.Util.option_map v.include_management_events (fun f ->
               Aws.Query.Pair ("IncludeManagementEvents", Boolean.to_query f))
         ; Aws.Util.option_map v.read_write_type (fun f ->
               Aws.Query.Pair ("ReadWriteType", ReadWriteType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "ExcludeManagementEventSources"
             , ExcludeManagementEventSources.to_json v.exclude_management_event_sources )
         ; Some ("DataResources", DataResources.to_json v.data_resources)
         ; Aws.Util.option_map v.include_management_events (fun f ->
               "IncludeManagementEvents", Boolean.to_json f)
         ; Aws.Util.option_map v.read_write_type (fun f ->
               "ReadWriteType", ReadWriteType.to_json f)
         ])

  let of_json j =
    { read_write_type =
        Aws.Util.option_map (Aws.Json.lookup j "ReadWriteType") ReadWriteType.of_json
    ; include_management_events =
        Aws.Util.option_map (Aws.Json.lookup j "IncludeManagementEvents") Boolean.of_json
    ; data_resources =
        DataResources.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DataResources"))
    ; exclude_management_event_sources =
        ExcludeManagementEventSources.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ExcludeManagementEventSources"))
    }
end

module EventSelectors = struct
  type t = EventSelector.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map EventSelector.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list EventSelector.to_query v

  let to_json v = `List (List.map EventSelector.to_json v)

  let of_json j = Aws.Json.to_list EventSelector.of_json j
end

module InsightType = struct
  type t = ApiCallRateInsight

  let str_to_t = [ "ApiCallRateInsight", ApiCallRateInsight ]

  let t_to_str = [ ApiCallRateInsight, "ApiCallRateInsight" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module InsightSelector = struct
  type t = { insight_type : InsightType.t option }

  let make ?insight_type () = { insight_type }

  let parse xml =
    Some
      { insight_type =
          Aws.Util.option_bind (Aws.Xml.member "InsightType" xml) InsightType.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.insight_type (fun f ->
               Aws.Query.Pair ("InsightType", InsightType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.insight_type (fun f ->
               "InsightType", InsightType.to_json f)
         ])

  let of_json j =
    { insight_type =
        Aws.Util.option_map (Aws.Json.lookup j "InsightType") InsightType.of_json
    }
end

module InsightSelectors = struct
  type t = InsightSelector.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map InsightSelector.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list InsightSelector.to_query v

  let to_json v = `List (List.map InsightSelector.to_json v)

  let of_json j = Aws.Json.to_list InsightSelector.of_json j
end

module PutInsightSelectorsResponse = struct
  type t =
    { trail_a_r_n : String.t option
    ; insight_selectors : InsightSelectors.t
    }

  let make ?trail_a_r_n ?(insight_selectors = []) () = { trail_a_r_n; insight_selectors }

  let parse xml =
    Some
      { trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
      ; insight_selectors =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "InsightSelectors" xml)
               InsightSelectors.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("InsightSelectors.member", InsightSelectors.to_query v.insight_selectors))
         ; Aws.Util.option_map v.trail_a_r_n (fun f ->
               Aws.Query.Pair ("TrailARN", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("InsightSelectors", InsightSelectors.to_json v.insight_selectors)
         ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
         ])

  let of_json j =
    { trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
    ; insight_selectors =
        InsightSelectors.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "InsightSelectors"))
    }
end

module TrailInfo = struct
  type t =
    { trail_a_r_n : String.t option
    ; name : String.t option
    ; home_region : String.t option
    }

  let make ?trail_a_r_n ?name ?home_region () = { trail_a_r_n; name; home_region }

  let parse xml =
    Some
      { trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
      ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; home_region = Aws.Util.option_bind (Aws.Xml.member "HomeRegion" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.home_region (fun f ->
               Aws.Query.Pair ("HomeRegion", String.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ; Aws.Util.option_map v.trail_a_r_n (fun f ->
               Aws.Query.Pair ("TrailARN", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.home_region (fun f -> "HomeRegion", String.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
         ])

  let of_json j =
    { trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
    ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; home_region = Aws.Util.option_map (Aws.Json.lookup j "HomeRegion") String.of_json
    }
end

module GetEventSelectorsResponse = struct
  type t =
    { trail_a_r_n : String.t option
    ; event_selectors : EventSelectors.t
    }

  let make ?trail_a_r_n ?(event_selectors = []) () = { trail_a_r_n; event_selectors }

  let parse xml =
    Some
      { trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
      ; event_selectors =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "EventSelectors" xml)
               EventSelectors.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("EventSelectors.member", EventSelectors.to_query v.event_selectors))
         ; Aws.Util.option_map v.trail_a_r_n (fun f ->
               Aws.Query.Pair ("TrailARN", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("EventSelectors", EventSelectors.to_json v.event_selectors)
         ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
         ])

  let of_json j =
    { trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
    ; event_selectors =
        EventSelectors.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EventSelectors"))
    }
end

module MaximumNumberOfTrailsExceededException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidNextTokenException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidS3PrefixException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidCloudWatchLogsLogGroupArnException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module OrganizationsNotInUseException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CloudTrailARNInvalidException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidEventCategoryException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InsightNotEnabledException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module RemoveTagsResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module EventCategory = struct
  type t = Insight

  let str_to_t = [ "insight", Insight ]

  let t_to_str = [ Insight, "insight" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module NotOrganizationMasterAccountException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module GetInsightSelectorsRequest = struct
  type t = { trail_name : String.t }

  let make ~trail_name () = { trail_name }

  let parse xml =
    Some
      { trail_name =
          Aws.Xml.required
            "TrailName"
            (Aws.Util.option_bind (Aws.Xml.member "TrailName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TrailName", String.to_query v.trail_name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("TrailName", String.to_json v.trail_name) ])

  let of_json j =
    { trail_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrailName"))
    }
end

module Resource = struct
  type t =
    { resource_type : String.t option
    ; resource_name : String.t option
    }

  let make ?resource_type ?resource_name () = { resource_type; resource_name }

  let parse xml =
    Some
      { resource_type =
          Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
      ; resource_name =
          Aws.Util.option_bind (Aws.Xml.member "ResourceName" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.resource_name (fun f ->
               Aws.Query.Pair ("ResourceName", String.to_query f))
         ; Aws.Util.option_map v.resource_type (fun f ->
               Aws.Query.Pair ("ResourceType", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.resource_name (fun f -> "ResourceName", String.to_json f)
         ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
         ])

  let of_json j =
    { resource_type =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
    ; resource_name =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceName") String.of_json
    }
end

module ResourceList = struct
  type t = Resource.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Resource.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Resource.to_query v

  let to_json v = `List (List.map Resource.to_json v)

  let of_json j = Aws.Json.to_list Resource.of_json j
end

module Event = struct
  type t =
    { event_id : String.t option
    ; event_name : String.t option
    ; read_only : String.t option
    ; access_key_id : String.t option
    ; event_time : DateTime.t option
    ; event_source : String.t option
    ; username : String.t option
    ; resources : ResourceList.t
    ; cloud_trail_event : String.t option
    }

  let make
      ?event_id
      ?event_name
      ?read_only
      ?access_key_id
      ?event_time
      ?event_source
      ?username
      ?(resources = [])
      ?cloud_trail_event
      () =
    { event_id
    ; event_name
    ; read_only
    ; access_key_id
    ; event_time
    ; event_source
    ; username
    ; resources
    ; cloud_trail_event
    }

  let parse xml =
    Some
      { event_id = Aws.Util.option_bind (Aws.Xml.member "EventId" xml) String.parse
      ; event_name = Aws.Util.option_bind (Aws.Xml.member "EventName" xml) String.parse
      ; read_only = Aws.Util.option_bind (Aws.Xml.member "ReadOnly" xml) String.parse
      ; access_key_id =
          Aws.Util.option_bind (Aws.Xml.member "AccessKeyId" xml) String.parse
      ; event_time = Aws.Util.option_bind (Aws.Xml.member "EventTime" xml) DateTime.parse
      ; event_source =
          Aws.Util.option_bind (Aws.Xml.member "EventSource" xml) String.parse
      ; username = Aws.Util.option_bind (Aws.Xml.member "Username" xml) String.parse
      ; resources =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Resources" xml) ResourceList.parse)
      ; cloud_trail_event =
          Aws.Util.option_bind (Aws.Xml.member "CloudTrailEvent" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cloud_trail_event (fun f ->
               Aws.Query.Pair ("CloudTrailEvent", String.to_query f))
         ; Some (Aws.Query.Pair ("Resources.member", ResourceList.to_query v.resources))
         ; Aws.Util.option_map v.username (fun f ->
               Aws.Query.Pair ("Username", String.to_query f))
         ; Aws.Util.option_map v.event_source (fun f ->
               Aws.Query.Pair ("EventSource", String.to_query f))
         ; Aws.Util.option_map v.event_time (fun f ->
               Aws.Query.Pair ("EventTime", DateTime.to_query f))
         ; Aws.Util.option_map v.access_key_id (fun f ->
               Aws.Query.Pair ("AccessKeyId", String.to_query f))
         ; Aws.Util.option_map v.read_only (fun f ->
               Aws.Query.Pair ("ReadOnly", String.to_query f))
         ; Aws.Util.option_map v.event_name (fun f ->
               Aws.Query.Pair ("EventName", String.to_query f))
         ; Aws.Util.option_map v.event_id (fun f ->
               Aws.Query.Pair ("EventId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cloud_trail_event (fun f ->
               "CloudTrailEvent", String.to_json f)
         ; Some ("Resources", ResourceList.to_json v.resources)
         ; Aws.Util.option_map v.username (fun f -> "Username", String.to_json f)
         ; Aws.Util.option_map v.event_source (fun f -> "EventSource", String.to_json f)
         ; Aws.Util.option_map v.event_time (fun f -> "EventTime", DateTime.to_json f)
         ; Aws.Util.option_map v.access_key_id (fun f -> "AccessKeyId", String.to_json f)
         ; Aws.Util.option_map v.read_only (fun f -> "ReadOnly", String.to_json f)
         ; Aws.Util.option_map v.event_name (fun f -> "EventName", String.to_json f)
         ; Aws.Util.option_map v.event_id (fun f -> "EventId", String.to_json f)
         ])

  let of_json j =
    { event_id = Aws.Util.option_map (Aws.Json.lookup j "EventId") String.of_json
    ; event_name = Aws.Util.option_map (Aws.Json.lookup j "EventName") String.of_json
    ; read_only = Aws.Util.option_map (Aws.Json.lookup j "ReadOnly") String.of_json
    ; access_key_id = Aws.Util.option_map (Aws.Json.lookup j "AccessKeyId") String.of_json
    ; event_time = Aws.Util.option_map (Aws.Json.lookup j "EventTime") DateTime.of_json
    ; event_source = Aws.Util.option_map (Aws.Json.lookup j "EventSource") String.of_json
    ; username = Aws.Util.option_map (Aws.Json.lookup j "Username") String.of_json
    ; resources =
        ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Resources"))
    ; cloud_trail_event =
        Aws.Util.option_map (Aws.Json.lookup j "CloudTrailEvent") String.of_json
    }
end

module EventsList = struct
  type t = Event.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Event.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Event.to_query v

  let to_json v = `List (List.map Event.to_json v)

  let of_json j = Aws.Json.to_list Event.of_json j
end

module LookupEventsResponse = struct
  type t =
    { events : EventsList.t
    ; next_token : String.t option
    }

  let make ?(events = []) ?next_token () = { events; next_token }

  let parse xml =
    Some
      { events =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Events" xml) EventsList.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Events.member", EventsList.to_query v.events))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Events", EventsList.to_json v.events)
         ])

  let of_json j =
    { events = EventsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Events"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ResourceNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module TagsLimitExceededException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module KmsKeyNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InsufficientS3BucketPolicyException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ResourceIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module InvalidHomeRegionException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidEventSelectorsException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InsufficientSnsTopicPolicyException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module UnsupportedOperationException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CloudTrailAccessNotEnabledException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidTimeRangeException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidParameterCombinationException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidMaxResultsException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CloudWatchLogsDeliveryUnavailableException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module OperationNotPermittedException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InsufficientEncryptionPolicyException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListPublicKeysRequest = struct
  type t =
    { start_time : DateTime.t option
    ; end_time : DateTime.t option
    ; next_token : String.t option
    }

  let make ?start_time ?end_time ?next_token () = { start_time; end_time; next_token }

  let parse xml =
    Some
      { start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
      ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.end_time (fun f ->
               Aws.Query.Pair ("EndTime", DateTime.to_query f))
         ; Aws.Util.option_map v.start_time (fun f ->
               Aws.Query.Pair ("StartTime", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
         ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
         ])

  let of_json j =
    { start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
    ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module TrailNameList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DescribeTrailsRequest = struct
  type t =
    { trail_name_list : TrailNameList.t
    ; include_shadow_trails : Boolean.t option
    }

  let make ?(trail_name_list = []) ?include_shadow_trails () =
    { trail_name_list; include_shadow_trails }

  let parse xml =
    Some
      { trail_name_list =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "trailNameList" xml)
               TrailNameList.parse)
      ; include_shadow_trails =
          Aws.Util.option_bind (Aws.Xml.member "includeShadowTrails" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.include_shadow_trails (fun f ->
               Aws.Query.Pair ("includeShadowTrails", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair
                ("trailNameList.member", TrailNameList.to_query v.trail_name_list))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.include_shadow_trails (fun f ->
               "includeShadowTrails", Boolean.to_json f)
         ; Some ("trailNameList", TrailNameList.to_json v.trail_name_list)
         ])

  let of_json j =
    { trail_name_list =
        TrailNameList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "trailNameList"))
    ; include_shadow_trails =
        Aws.Util.option_map (Aws.Json.lookup j "includeShadowTrails") Boolean.of_json
    }
end

module S3BucketDoesNotExistException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module Trails = struct
  type t = TrailInfo.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map TrailInfo.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list TrailInfo.to_query v

  let to_json v = `List (List.map TrailInfo.to_json v)

  let of_json j = Aws.Json.to_list TrailInfo.of_json j
end

module InvalidTrailNameException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidLookupAttributesException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module UpdateTrailRequest = struct
  type t =
    { name : String.t
    ; s3_bucket_name : String.t option
    ; s3_key_prefix : String.t option
    ; sns_topic_name : String.t option
    ; include_global_service_events : Boolean.t option
    ; is_multi_region_trail : Boolean.t option
    ; enable_log_file_validation : Boolean.t option
    ; cloud_watch_logs_log_group_arn : String.t option
    ; cloud_watch_logs_role_arn : String.t option
    ; kms_key_id : String.t option
    ; is_organization_trail : Boolean.t option
    }

  let make
      ~name
      ?s3_bucket_name
      ?s3_key_prefix
      ?sns_topic_name
      ?include_global_service_events
      ?is_multi_region_trail
      ?enable_log_file_validation
      ?cloud_watch_logs_log_group_arn
      ?cloud_watch_logs_role_arn
      ?kms_key_id
      ?is_organization_trail
      () =
    { name
    ; s3_bucket_name
    ; s3_key_prefix
    ; sns_topic_name
    ; include_global_service_events
    ; is_multi_region_trail
    ; enable_log_file_validation
    ; cloud_watch_logs_log_group_arn
    ; cloud_watch_logs_role_arn
    ; kms_key_id
    ; is_organization_trail
    }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; s3_bucket_name =
          Aws.Util.option_bind (Aws.Xml.member "S3BucketName" xml) String.parse
      ; s3_key_prefix =
          Aws.Util.option_bind (Aws.Xml.member "S3KeyPrefix" xml) String.parse
      ; sns_topic_name =
          Aws.Util.option_bind (Aws.Xml.member "SnsTopicName" xml) String.parse
      ; include_global_service_events =
          Aws.Util.option_bind
            (Aws.Xml.member "IncludeGlobalServiceEvents" xml)
            Boolean.parse
      ; is_multi_region_trail =
          Aws.Util.option_bind (Aws.Xml.member "IsMultiRegionTrail" xml) Boolean.parse
      ; enable_log_file_validation =
          Aws.Util.option_bind
            (Aws.Xml.member "EnableLogFileValidation" xml)
            Boolean.parse
      ; cloud_watch_logs_log_group_arn =
          Aws.Util.option_bind
            (Aws.Xml.member "CloudWatchLogsLogGroupArn" xml)
            String.parse
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
         ; Aws.Util.option_map v.enable_log_file_validation (fun f ->
               Aws.Query.Pair ("EnableLogFileValidation", Boolean.to_query f))
         ; Aws.Util.option_map v.is_multi_region_trail (fun f ->
               Aws.Query.Pair ("IsMultiRegionTrail", Boolean.to_query f))
         ; Aws.Util.option_map v.include_global_service_events (fun f ->
               Aws.Query.Pair ("IncludeGlobalServiceEvents", Boolean.to_query f))
         ; Aws.Util.option_map v.sns_topic_name (fun f ->
               Aws.Query.Pair ("SnsTopicName", String.to_query f))
         ; Aws.Util.option_map v.s3_key_prefix (fun f ->
               Aws.Query.Pair ("S3KeyPrefix", String.to_query f))
         ; Aws.Util.option_map v.s3_bucket_name (fun f ->
               Aws.Query.Pair ("S3BucketName", String.to_query f))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
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
         ; Aws.Util.option_map v.enable_log_file_validation (fun f ->
               "EnableLogFileValidation", Boolean.to_json f)
         ; Aws.Util.option_map v.is_multi_region_trail (fun f ->
               "IsMultiRegionTrail", Boolean.to_json f)
         ; Aws.Util.option_map v.include_global_service_events (fun f ->
               "IncludeGlobalServiceEvents", Boolean.to_json f)
         ; Aws.Util.option_map v.sns_topic_name (fun f ->
               "SnsTopicName", String.to_json f)
         ; Aws.Util.option_map v.s3_key_prefix (fun f -> "S3KeyPrefix", String.to_json f)
         ; Aws.Util.option_map v.s3_bucket_name (fun f ->
               "S3BucketName", String.to_json f)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; s3_bucket_name =
        Aws.Util.option_map (Aws.Json.lookup j "S3BucketName") String.of_json
    ; s3_key_prefix = Aws.Util.option_map (Aws.Json.lookup j "S3KeyPrefix") String.of_json
    ; sns_topic_name =
        Aws.Util.option_map (Aws.Json.lookup j "SnsTopicName") String.of_json
    ; include_global_service_events =
        Aws.Util.option_map
          (Aws.Json.lookup j "IncludeGlobalServiceEvents")
          Boolean.of_json
    ; is_multi_region_trail =
        Aws.Util.option_map (Aws.Json.lookup j "IsMultiRegionTrail") Boolean.of_json
    ; enable_log_file_validation =
        Aws.Util.option_map (Aws.Json.lookup j "EnableLogFileValidation") Boolean.of_json
    ; cloud_watch_logs_log_group_arn =
        Aws.Util.option_map (Aws.Json.lookup j "CloudWatchLogsLogGroupArn") String.of_json
    ; cloud_watch_logs_role_arn =
        Aws.Util.option_map (Aws.Json.lookup j "CloudWatchLogsRoleArn") String.of_json
    ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
    ; is_organization_trail =
        Aws.Util.option_map (Aws.Json.lookup j "IsOrganizationTrail") Boolean.of_json
    }
end

module GetTrailStatusRequest = struct
  type t = { name : String.t }

  let make ~name () = { name }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Name", String.to_query v.name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Name", String.to_json v.name) ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name")) }
end

module AddTagsRequest = struct
  type t =
    { resource_id : String.t
    ; tags_list : TagsList.t
    }

  let make ~resource_id ?(tags_list = []) () = { resource_id; tags_list }

  let parse xml =
    Some
      { resource_id =
          Aws.Xml.required
            "ResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse)
      ; tags_list =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "TagsList" xml) TagsList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TagsList.member", TagsList.to_query v.tags_list))
         ; Some (Aws.Query.Pair ("ResourceId", String.to_query v.resource_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TagsList", TagsList.to_json v.tags_list)
         ; Some ("ResourceId", String.to_json v.resource_id)
         ])

  let of_json j =
    { resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
    ; tags_list = TagsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagsList"))
    }
end

module PutEventSelectorsRequest = struct
  type t =
    { trail_name : String.t
    ; event_selectors : EventSelectors.t
    }

  let make ~trail_name ~event_selectors () = { trail_name; event_selectors }

  let parse xml =
    Some
      { trail_name =
          Aws.Xml.required
            "TrailName"
            (Aws.Util.option_bind (Aws.Xml.member "TrailName" xml) String.parse)
      ; event_selectors =
          Aws.Xml.required
            "EventSelectors"
            (Aws.Util.option_bind
               (Aws.Xml.member "EventSelectors" xml)
               EventSelectors.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("EventSelectors.member", EventSelectors.to_query v.event_selectors))
         ; Some (Aws.Query.Pair ("TrailName", String.to_query v.trail_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("EventSelectors", EventSelectors.to_json v.event_selectors)
         ; Some ("TrailName", String.to_json v.trail_name)
         ])

  let of_json j =
    { trail_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrailName"))
    ; event_selectors =
        EventSelectors.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EventSelectors"))
    }
end

module GetEventSelectorsRequest = struct
  type t = { trail_name : String.t }

  let make ~trail_name () = { trail_name }

  let parse xml =
    Some
      { trail_name =
          Aws.Xml.required
            "TrailName"
            (Aws.Util.option_bind (Aws.Xml.member "TrailName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TrailName", String.to_query v.trail_name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("TrailName", String.to_json v.trail_name) ])

  let of_json j =
    { trail_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrailName"))
    }
end

module PutInsightSelectorsRequest = struct
  type t =
    { trail_name : String.t
    ; insight_selectors : InsightSelectors.t
    }

  let make ~trail_name ~insight_selectors () = { trail_name; insight_selectors }

  let parse xml =
    Some
      { trail_name =
          Aws.Xml.required
            "TrailName"
            (Aws.Util.option_bind (Aws.Xml.member "TrailName" xml) String.parse)
      ; insight_selectors =
          Aws.Xml.required
            "InsightSelectors"
            (Aws.Util.option_bind
               (Aws.Xml.member "InsightSelectors" xml)
               InsightSelectors.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("InsightSelectors.member", InsightSelectors.to_query v.insight_selectors))
         ; Some (Aws.Query.Pair ("TrailName", String.to_query v.trail_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("InsightSelectors", InsightSelectors.to_json v.insight_selectors)
         ; Some ("TrailName", String.to_json v.trail_name)
         ])

  let of_json j =
    { trail_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrailName"))
    ; insight_selectors =
        InsightSelectors.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "InsightSelectors"))
    }
end

module OrganizationNotInAllFeaturesModeException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListTrailsRequest = struct
  type t = { next_token : String.t option }

  let make ?next_token () = { next_token }

  let parse xml =
    Some
      { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f) ])

  let of_json j =
    { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json }
end

module StartLoggingResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module AddTagsResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ResourceTagList = struct
  type t = ResourceTag.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map ResourceTag.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ResourceTag.to_query v

  let to_json v = `List (List.map ResourceTag.to_json v)

  let of_json j = Aws.Json.to_list ResourceTag.of_json j
end

module CreateTrailRequest = struct
  type t =
    { name : String.t
    ; s3_bucket_name : String.t
    ; s3_key_prefix : String.t option
    ; sns_topic_name : String.t option
    ; include_global_service_events : Boolean.t option
    ; is_multi_region_trail : Boolean.t option
    ; enable_log_file_validation : Boolean.t option
    ; cloud_watch_logs_log_group_arn : String.t option
    ; cloud_watch_logs_role_arn : String.t option
    ; kms_key_id : String.t option
    ; is_organization_trail : Boolean.t option
    ; tags_list : TagsList.t
    }

  let make
      ~name
      ~s3_bucket_name
      ?s3_key_prefix
      ?sns_topic_name
      ?include_global_service_events
      ?is_multi_region_trail
      ?enable_log_file_validation
      ?cloud_watch_logs_log_group_arn
      ?cloud_watch_logs_role_arn
      ?kms_key_id
      ?is_organization_trail
      ?(tags_list = [])
      () =
    { name
    ; s3_bucket_name
    ; s3_key_prefix
    ; sns_topic_name
    ; include_global_service_events
    ; is_multi_region_trail
    ; enable_log_file_validation
    ; cloud_watch_logs_log_group_arn
    ; cloud_watch_logs_role_arn
    ; kms_key_id
    ; is_organization_trail
    ; tags_list
    }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; s3_bucket_name =
          Aws.Xml.required
            "S3BucketName"
            (Aws.Util.option_bind (Aws.Xml.member "S3BucketName" xml) String.parse)
      ; s3_key_prefix =
          Aws.Util.option_bind (Aws.Xml.member "S3KeyPrefix" xml) String.parse
      ; sns_topic_name =
          Aws.Util.option_bind (Aws.Xml.member "SnsTopicName" xml) String.parse
      ; include_global_service_events =
          Aws.Util.option_bind
            (Aws.Xml.member "IncludeGlobalServiceEvents" xml)
            Boolean.parse
      ; is_multi_region_trail =
          Aws.Util.option_bind (Aws.Xml.member "IsMultiRegionTrail" xml) Boolean.parse
      ; enable_log_file_validation =
          Aws.Util.option_bind
            (Aws.Xml.member "EnableLogFileValidation" xml)
            Boolean.parse
      ; cloud_watch_logs_log_group_arn =
          Aws.Util.option_bind
            (Aws.Xml.member "CloudWatchLogsLogGroupArn" xml)
            String.parse
      ; cloud_watch_logs_role_arn =
          Aws.Util.option_bind (Aws.Xml.member "CloudWatchLogsRoleArn" xml) String.parse
      ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
      ; is_organization_trail =
          Aws.Util.option_bind (Aws.Xml.member "IsOrganizationTrail" xml) Boolean.parse
      ; tags_list =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "TagsList" xml) TagsList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TagsList.member", TagsList.to_query v.tags_list))
         ; Aws.Util.option_map v.is_organization_trail (fun f ->
               Aws.Query.Pair ("IsOrganizationTrail", Boolean.to_query f))
         ; Aws.Util.option_map v.kms_key_id (fun f ->
               Aws.Query.Pair ("KmsKeyId", String.to_query f))
         ; Aws.Util.option_map v.cloud_watch_logs_role_arn (fun f ->
               Aws.Query.Pair ("CloudWatchLogsRoleArn", String.to_query f))
         ; Aws.Util.option_map v.cloud_watch_logs_log_group_arn (fun f ->
               Aws.Query.Pair ("CloudWatchLogsLogGroupArn", String.to_query f))
         ; Aws.Util.option_map v.enable_log_file_validation (fun f ->
               Aws.Query.Pair ("EnableLogFileValidation", Boolean.to_query f))
         ; Aws.Util.option_map v.is_multi_region_trail (fun f ->
               Aws.Query.Pair ("IsMultiRegionTrail", Boolean.to_query f))
         ; Aws.Util.option_map v.include_global_service_events (fun f ->
               Aws.Query.Pair ("IncludeGlobalServiceEvents", Boolean.to_query f))
         ; Aws.Util.option_map v.sns_topic_name (fun f ->
               Aws.Query.Pair ("SnsTopicName", String.to_query f))
         ; Aws.Util.option_map v.s3_key_prefix (fun f ->
               Aws.Query.Pair ("S3KeyPrefix", String.to_query f))
         ; Some (Aws.Query.Pair ("S3BucketName", String.to_query v.s3_bucket_name))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TagsList", TagsList.to_json v.tags_list)
         ; Aws.Util.option_map v.is_organization_trail (fun f ->
               "IsOrganizationTrail", Boolean.to_json f)
         ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
         ; Aws.Util.option_map v.cloud_watch_logs_role_arn (fun f ->
               "CloudWatchLogsRoleArn", String.to_json f)
         ; Aws.Util.option_map v.cloud_watch_logs_log_group_arn (fun f ->
               "CloudWatchLogsLogGroupArn", String.to_json f)
         ; Aws.Util.option_map v.enable_log_file_validation (fun f ->
               "EnableLogFileValidation", Boolean.to_json f)
         ; Aws.Util.option_map v.is_multi_region_trail (fun f ->
               "IsMultiRegionTrail", Boolean.to_json f)
         ; Aws.Util.option_map v.include_global_service_events (fun f ->
               "IncludeGlobalServiceEvents", Boolean.to_json f)
         ; Aws.Util.option_map v.sns_topic_name (fun f ->
               "SnsTopicName", String.to_json f)
         ; Aws.Util.option_map v.s3_key_prefix (fun f -> "S3KeyPrefix", String.to_json f)
         ; Some ("S3BucketName", String.to_json v.s3_bucket_name)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; s3_bucket_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3BucketName"))
    ; s3_key_prefix = Aws.Util.option_map (Aws.Json.lookup j "S3KeyPrefix") String.of_json
    ; sns_topic_name =
        Aws.Util.option_map (Aws.Json.lookup j "SnsTopicName") String.of_json
    ; include_global_service_events =
        Aws.Util.option_map
          (Aws.Json.lookup j "IncludeGlobalServiceEvents")
          Boolean.of_json
    ; is_multi_region_trail =
        Aws.Util.option_map (Aws.Json.lookup j "IsMultiRegionTrail") Boolean.of_json
    ; enable_log_file_validation =
        Aws.Util.option_map (Aws.Json.lookup j "EnableLogFileValidation") Boolean.of_json
    ; cloud_watch_logs_log_group_arn =
        Aws.Util.option_map (Aws.Json.lookup j "CloudWatchLogsLogGroupArn") String.of_json
    ; cloud_watch_logs_role_arn =
        Aws.Util.option_map (Aws.Json.lookup j "CloudWatchLogsRoleArn") String.of_json
    ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
    ; is_organization_trail =
        Aws.Util.option_map (Aws.Json.lookup j "IsOrganizationTrail") Boolean.of_json
    ; tags_list = TagsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagsList"))
    }
end

module UpdateTrailResponse = struct
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
      ; s3_key_prefix =
          Aws.Util.option_bind (Aws.Xml.member "S3KeyPrefix" xml) String.parse
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
          Aws.Util.option_bind
            (Aws.Xml.member "LogFileValidationEnabled" xml)
            Boolean.parse
      ; cloud_watch_logs_log_group_arn =
          Aws.Util.option_bind
            (Aws.Xml.member "CloudWatchLogsLogGroupArn" xml)
            String.parse
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
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
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
         ; Aws.Util.option_map v.sns_topic_a_r_n (fun f ->
               "SnsTopicARN", String.to_json f)
         ; Aws.Util.option_map v.sns_topic_name (fun f ->
               "SnsTopicName", String.to_json f)
         ; Aws.Util.option_map v.s3_key_prefix (fun f -> "S3KeyPrefix", String.to_json f)
         ; Aws.Util.option_map v.s3_bucket_name (fun f ->
               "S3BucketName", String.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ])

  let of_json j =
    { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; s3_bucket_name =
        Aws.Util.option_map (Aws.Json.lookup j "S3BucketName") String.of_json
    ; s3_key_prefix = Aws.Util.option_map (Aws.Json.lookup j "S3KeyPrefix") String.of_json
    ; sns_topic_name =
        Aws.Util.option_map (Aws.Json.lookup j "SnsTopicName") String.of_json
    ; sns_topic_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "SnsTopicARN") String.of_json
    ; include_global_service_events =
        Aws.Util.option_map
          (Aws.Json.lookup j "IncludeGlobalServiceEvents")
          Boolean.of_json
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
end

module InvalidTagParameterException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module TrailNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module KmsKeyDisabledException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module GetTrailResponse = struct
  type t = { trail : Trail.t option }

  let make ?trail () = { trail }

  let parse xml =
    Some { trail = Aws.Util.option_bind (Aws.Xml.member "Trail" xml) Trail.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.trail (fun f ->
               Aws.Query.Pair ("Trail", Trail.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.trail (fun f -> "Trail", Trail.to_json f) ])

  let of_json j =
    { trail = Aws.Util.option_map (Aws.Json.lookup j "Trail") Trail.of_json }
end

module DeleteTrailRequest = struct
  type t = { name : String.t }

  let make ~name () = { name }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Name", String.to_query v.name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Name", String.to_json v.name) ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name")) }
end

module ListTrailsResponse = struct
  type t =
    { trails : Trails.t
    ; next_token : String.t option
    }

  let make ?(trails = []) ?next_token () = { trails; next_token }

  let parse xml =
    Some
      { trails =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Trails" xml) Trails.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Trails.member", Trails.to_query v.trails))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Trails", Trails.to_json v.trails)
         ])

  let of_json j =
    { trails = Trails.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Trails"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module TrailNotProvidedException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module LookupAttributesList = struct
  type t = LookupAttribute.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map LookupAttribute.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list LookupAttribute.to_query v

  let to_json v = `List (List.map LookupAttribute.to_json v)

  let of_json j = Aws.Json.to_list LookupAttribute.of_json j
end

module InvalidInsightSelectorsException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module KmsException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidKmsKeyIdException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module GetTrailStatusResponse = struct
  type t =
    { is_logging : Boolean.t option
    ; latest_delivery_error : String.t option
    ; latest_notification_error : String.t option
    ; latest_delivery_time : DateTime.t option
    ; latest_notification_time : DateTime.t option
    ; start_logging_time : DateTime.t option
    ; stop_logging_time : DateTime.t option
    ; latest_cloud_watch_logs_delivery_error : String.t option
    ; latest_cloud_watch_logs_delivery_time : DateTime.t option
    ; latest_digest_delivery_time : DateTime.t option
    ; latest_digest_delivery_error : String.t option
    ; latest_delivery_attempt_time : String.t option
    ; latest_notification_attempt_time : String.t option
    ; latest_notification_attempt_succeeded : String.t option
    ; latest_delivery_attempt_succeeded : String.t option
    ; time_logging_started : String.t option
    ; time_logging_stopped : String.t option
    }

  let make
      ?is_logging
      ?latest_delivery_error
      ?latest_notification_error
      ?latest_delivery_time
      ?latest_notification_time
      ?start_logging_time
      ?stop_logging_time
      ?latest_cloud_watch_logs_delivery_error
      ?latest_cloud_watch_logs_delivery_time
      ?latest_digest_delivery_time
      ?latest_digest_delivery_error
      ?latest_delivery_attempt_time
      ?latest_notification_attempt_time
      ?latest_notification_attempt_succeeded
      ?latest_delivery_attempt_succeeded
      ?time_logging_started
      ?time_logging_stopped
      () =
    { is_logging
    ; latest_delivery_error
    ; latest_notification_error
    ; latest_delivery_time
    ; latest_notification_time
    ; start_logging_time
    ; stop_logging_time
    ; latest_cloud_watch_logs_delivery_error
    ; latest_cloud_watch_logs_delivery_time
    ; latest_digest_delivery_time
    ; latest_digest_delivery_error
    ; latest_delivery_attempt_time
    ; latest_notification_attempt_time
    ; latest_notification_attempt_succeeded
    ; latest_delivery_attempt_succeeded
    ; time_logging_started
    ; time_logging_stopped
    }

  let parse xml =
    Some
      { is_logging = Aws.Util.option_bind (Aws.Xml.member "IsLogging" xml) Boolean.parse
      ; latest_delivery_error =
          Aws.Util.option_bind (Aws.Xml.member "LatestDeliveryError" xml) String.parse
      ; latest_notification_error =
          Aws.Util.option_bind (Aws.Xml.member "LatestNotificationError" xml) String.parse
      ; latest_delivery_time =
          Aws.Util.option_bind (Aws.Xml.member "LatestDeliveryTime" xml) DateTime.parse
      ; latest_notification_time =
          Aws.Util.option_bind
            (Aws.Xml.member "LatestNotificationTime" xml)
            DateTime.parse
      ; start_logging_time =
          Aws.Util.option_bind (Aws.Xml.member "StartLoggingTime" xml) DateTime.parse
      ; stop_logging_time =
          Aws.Util.option_bind (Aws.Xml.member "StopLoggingTime" xml) DateTime.parse
      ; latest_cloud_watch_logs_delivery_error =
          Aws.Util.option_bind
            (Aws.Xml.member "LatestCloudWatchLogsDeliveryError" xml)
            String.parse
      ; latest_cloud_watch_logs_delivery_time =
          Aws.Util.option_bind
            (Aws.Xml.member "LatestCloudWatchLogsDeliveryTime" xml)
            DateTime.parse
      ; latest_digest_delivery_time =
          Aws.Util.option_bind
            (Aws.Xml.member "LatestDigestDeliveryTime" xml)
            DateTime.parse
      ; latest_digest_delivery_error =
          Aws.Util.option_bind
            (Aws.Xml.member "LatestDigestDeliveryError" xml)
            String.parse
      ; latest_delivery_attempt_time =
          Aws.Util.option_bind
            (Aws.Xml.member "LatestDeliveryAttemptTime" xml)
            String.parse
      ; latest_notification_attempt_time =
          Aws.Util.option_bind
            (Aws.Xml.member "LatestNotificationAttemptTime" xml)
            String.parse
      ; latest_notification_attempt_succeeded =
          Aws.Util.option_bind
            (Aws.Xml.member "LatestNotificationAttemptSucceeded" xml)
            String.parse
      ; latest_delivery_attempt_succeeded =
          Aws.Util.option_bind
            (Aws.Xml.member "LatestDeliveryAttemptSucceeded" xml)
            String.parse
      ; time_logging_started =
          Aws.Util.option_bind (Aws.Xml.member "TimeLoggingStarted" xml) String.parse
      ; time_logging_stopped =
          Aws.Util.option_bind (Aws.Xml.member "TimeLoggingStopped" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.time_logging_stopped (fun f ->
               Aws.Query.Pair ("TimeLoggingStopped", String.to_query f))
         ; Aws.Util.option_map v.time_logging_started (fun f ->
               Aws.Query.Pair ("TimeLoggingStarted", String.to_query f))
         ; Aws.Util.option_map v.latest_delivery_attempt_succeeded (fun f ->
               Aws.Query.Pair ("LatestDeliveryAttemptSucceeded", String.to_query f))
         ; Aws.Util.option_map v.latest_notification_attempt_succeeded (fun f ->
               Aws.Query.Pair ("LatestNotificationAttemptSucceeded", String.to_query f))
         ; Aws.Util.option_map v.latest_notification_attempt_time (fun f ->
               Aws.Query.Pair ("LatestNotificationAttemptTime", String.to_query f))
         ; Aws.Util.option_map v.latest_delivery_attempt_time (fun f ->
               Aws.Query.Pair ("LatestDeliveryAttemptTime", String.to_query f))
         ; Aws.Util.option_map v.latest_digest_delivery_error (fun f ->
               Aws.Query.Pair ("LatestDigestDeliveryError", String.to_query f))
         ; Aws.Util.option_map v.latest_digest_delivery_time (fun f ->
               Aws.Query.Pair ("LatestDigestDeliveryTime", DateTime.to_query f))
         ; Aws.Util.option_map v.latest_cloud_watch_logs_delivery_time (fun f ->
               Aws.Query.Pair ("LatestCloudWatchLogsDeliveryTime", DateTime.to_query f))
         ; Aws.Util.option_map v.latest_cloud_watch_logs_delivery_error (fun f ->
               Aws.Query.Pair ("LatestCloudWatchLogsDeliveryError", String.to_query f))
         ; Aws.Util.option_map v.stop_logging_time (fun f ->
               Aws.Query.Pair ("StopLoggingTime", DateTime.to_query f))
         ; Aws.Util.option_map v.start_logging_time (fun f ->
               Aws.Query.Pair ("StartLoggingTime", DateTime.to_query f))
         ; Aws.Util.option_map v.latest_notification_time (fun f ->
               Aws.Query.Pair ("LatestNotificationTime", DateTime.to_query f))
         ; Aws.Util.option_map v.latest_delivery_time (fun f ->
               Aws.Query.Pair ("LatestDeliveryTime", DateTime.to_query f))
         ; Aws.Util.option_map v.latest_notification_error (fun f ->
               Aws.Query.Pair ("LatestNotificationError", String.to_query f))
         ; Aws.Util.option_map v.latest_delivery_error (fun f ->
               Aws.Query.Pair ("LatestDeliveryError", String.to_query f))
         ; Aws.Util.option_map v.is_logging (fun f ->
               Aws.Query.Pair ("IsLogging", Boolean.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.time_logging_stopped (fun f ->
               "TimeLoggingStopped", String.to_json f)
         ; Aws.Util.option_map v.time_logging_started (fun f ->
               "TimeLoggingStarted", String.to_json f)
         ; Aws.Util.option_map v.latest_delivery_attempt_succeeded (fun f ->
               "LatestDeliveryAttemptSucceeded", String.to_json f)
         ; Aws.Util.option_map v.latest_notification_attempt_succeeded (fun f ->
               "LatestNotificationAttemptSucceeded", String.to_json f)
         ; Aws.Util.option_map v.latest_notification_attempt_time (fun f ->
               "LatestNotificationAttemptTime", String.to_json f)
         ; Aws.Util.option_map v.latest_delivery_attempt_time (fun f ->
               "LatestDeliveryAttemptTime", String.to_json f)
         ; Aws.Util.option_map v.latest_digest_delivery_error (fun f ->
               "LatestDigestDeliveryError", String.to_json f)
         ; Aws.Util.option_map v.latest_digest_delivery_time (fun f ->
               "LatestDigestDeliveryTime", DateTime.to_json f)
         ; Aws.Util.option_map v.latest_cloud_watch_logs_delivery_time (fun f ->
               "LatestCloudWatchLogsDeliveryTime", DateTime.to_json f)
         ; Aws.Util.option_map v.latest_cloud_watch_logs_delivery_error (fun f ->
               "LatestCloudWatchLogsDeliveryError", String.to_json f)
         ; Aws.Util.option_map v.stop_logging_time (fun f ->
               "StopLoggingTime", DateTime.to_json f)
         ; Aws.Util.option_map v.start_logging_time (fun f ->
               "StartLoggingTime", DateTime.to_json f)
         ; Aws.Util.option_map v.latest_notification_time (fun f ->
               "LatestNotificationTime", DateTime.to_json f)
         ; Aws.Util.option_map v.latest_delivery_time (fun f ->
               "LatestDeliveryTime", DateTime.to_json f)
         ; Aws.Util.option_map v.latest_notification_error (fun f ->
               "LatestNotificationError", String.to_json f)
         ; Aws.Util.option_map v.latest_delivery_error (fun f ->
               "LatestDeliveryError", String.to_json f)
         ; Aws.Util.option_map v.is_logging (fun f -> "IsLogging", Boolean.to_json f)
         ])

  let of_json j =
    { is_logging = Aws.Util.option_map (Aws.Json.lookup j "IsLogging") Boolean.of_json
    ; latest_delivery_error =
        Aws.Util.option_map (Aws.Json.lookup j "LatestDeliveryError") String.of_json
    ; latest_notification_error =
        Aws.Util.option_map (Aws.Json.lookup j "LatestNotificationError") String.of_json
    ; latest_delivery_time =
        Aws.Util.option_map (Aws.Json.lookup j "LatestDeliveryTime") DateTime.of_json
    ; latest_notification_time =
        Aws.Util.option_map (Aws.Json.lookup j "LatestNotificationTime") DateTime.of_json
    ; start_logging_time =
        Aws.Util.option_map (Aws.Json.lookup j "StartLoggingTime") DateTime.of_json
    ; stop_logging_time =
        Aws.Util.option_map (Aws.Json.lookup j "StopLoggingTime") DateTime.of_json
    ; latest_cloud_watch_logs_delivery_error =
        Aws.Util.option_map
          (Aws.Json.lookup j "LatestCloudWatchLogsDeliveryError")
          String.of_json
    ; latest_cloud_watch_logs_delivery_time =
        Aws.Util.option_map
          (Aws.Json.lookup j "LatestCloudWatchLogsDeliveryTime")
          DateTime.of_json
    ; latest_digest_delivery_time =
        Aws.Util.option_map
          (Aws.Json.lookup j "LatestDigestDeliveryTime")
          DateTime.of_json
    ; latest_digest_delivery_error =
        Aws.Util.option_map (Aws.Json.lookup j "LatestDigestDeliveryError") String.of_json
    ; latest_delivery_attempt_time =
        Aws.Util.option_map (Aws.Json.lookup j "LatestDeliveryAttemptTime") String.of_json
    ; latest_notification_attempt_time =
        Aws.Util.option_map
          (Aws.Json.lookup j "LatestNotificationAttemptTime")
          String.of_json
    ; latest_notification_attempt_succeeded =
        Aws.Util.option_map
          (Aws.Json.lookup j "LatestNotificationAttemptSucceeded")
          String.of_json
    ; latest_delivery_attempt_succeeded =
        Aws.Util.option_map
          (Aws.Json.lookup j "LatestDeliveryAttemptSucceeded")
          String.of_json
    ; time_logging_started =
        Aws.Util.option_map (Aws.Json.lookup j "TimeLoggingStarted") String.of_json
    ; time_logging_stopped =
        Aws.Util.option_map (Aws.Json.lookup j "TimeLoggingStopped") String.of_json
    }
end

module StopLoggingResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidTokenException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module LookupEventsRequest = struct
  type t =
    { lookup_attributes : LookupAttributesList.t
    ; start_time : DateTime.t option
    ; end_time : DateTime.t option
    ; event_category : EventCategory.t option
    ; max_results : Integer.t option
    ; next_token : String.t option
    }

  let make
      ?(lookup_attributes = [])
      ?start_time
      ?end_time
      ?event_category
      ?max_results
      ?next_token
      () =
    { lookup_attributes; start_time; end_time; event_category; max_results; next_token }

  let parse xml =
    Some
      { lookup_attributes =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LookupAttributes" xml)
               LookupAttributesList.parse)
      ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
      ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
      ; event_category =
          Aws.Util.option_bind (Aws.Xml.member "EventCategory" xml) EventCategory.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.event_category (fun f ->
               Aws.Query.Pair ("EventCategory", EventCategory.to_query f))
         ; Aws.Util.option_map v.end_time (fun f ->
               Aws.Query.Pair ("EndTime", DateTime.to_query f))
         ; Aws.Util.option_map v.start_time (fun f ->
               Aws.Query.Pair ("StartTime", DateTime.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "LookupAttributes.member"
                , LookupAttributesList.to_query v.lookup_attributes ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.event_category (fun f ->
               "EventCategory", EventCategory.to_json f)
         ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
         ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
         ; Some ("LookupAttributes", LookupAttributesList.to_json v.lookup_attributes)
         ])

  let of_json j =
    { lookup_attributes =
        LookupAttributesList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LookupAttributes"))
    ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
    ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
    ; event_category =
        Aws.Util.option_map (Aws.Json.lookup j "EventCategory") EventCategory.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module InsufficientDependencyServiceAccessPermissionException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module PutEventSelectorsResponse = struct
  type t =
    { trail_a_r_n : String.t option
    ; event_selectors : EventSelectors.t
    }

  let make ?trail_a_r_n ?(event_selectors = []) () = { trail_a_r_n; event_selectors }

  let parse xml =
    Some
      { trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
      ; event_selectors =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "EventSelectors" xml)
               EventSelectors.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("EventSelectors.member", EventSelectors.to_query v.event_selectors))
         ; Aws.Util.option_map v.trail_a_r_n (fun f ->
               Aws.Query.Pair ("TrailARN", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("EventSelectors", EventSelectors.to_json v.event_selectors)
         ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
         ])

  let of_json j =
    { trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
    ; event_selectors =
        EventSelectors.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EventSelectors"))
    }
end

module InvalidCloudWatchLogsRoleArnException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module RemoveTagsRequest = struct
  type t =
    { resource_id : String.t
    ; tags_list : TagsList.t
    }

  let make ~resource_id ?(tags_list = []) () = { resource_id; tags_list }

  let parse xml =
    Some
      { resource_id =
          Aws.Xml.required
            "ResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse)
      ; tags_list =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "TagsList" xml) TagsList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TagsList.member", TagsList.to_query v.tags_list))
         ; Some (Aws.Query.Pair ("ResourceId", String.to_query v.resource_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TagsList", TagsList.to_json v.tags_list)
         ; Some ("ResourceId", String.to_json v.resource_id)
         ])

  let of_json j =
    { resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
    ; tags_list = TagsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagsList"))
    }
end

module InvalidS3BucketNameException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ResourceTypeNotSupportedException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module StartLoggingRequest = struct
  type t = { name : String.t }

  let make ~name () = { name }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Name", String.to_query v.name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Name", String.to_json v.name) ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name")) }
end

module TrailAlreadyExistsException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListTagsResponse = struct
  type t =
    { resource_tag_list : ResourceTagList.t
    ; next_token : String.t option
    }

  let make ?(resource_tag_list = []) ?next_token () = { resource_tag_list; next_token }

  let parse xml =
    Some
      { resource_tag_list =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceTagList" xml)
               ResourceTagList.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ResourceTagList.member", ResourceTagList.to_query v.resource_tag_list))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("ResourceTagList", ResourceTagList.to_json v.resource_tag_list)
         ])

  let of_json j =
    { resource_tag_list =
        ResourceTagList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTagList"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module StopLoggingRequest = struct
  type t = { name : String.t }

  let make ~name () = { name }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Name", String.to_query v.name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Name", String.to_json v.name) ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name")) }
end

module ListTagsRequest = struct
  type t =
    { resource_id_list : ResourceIdList.t
    ; next_token : String.t option
    }

  let make ~resource_id_list ?next_token () = { resource_id_list; next_token }

  let parse xml =
    Some
      { resource_id_list =
          Aws.Xml.required
            "ResourceIdList"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceIdList" xml)
               ResourceIdList.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ResourceIdList.member", ResourceIdList.to_query v.resource_id_list))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("ResourceIdList", ResourceIdList.to_json v.resource_id_list)
         ])

  let of_json j =
    { resource_id_list =
        ResourceIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIdList"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module GetInsightSelectorsResponse = struct
  type t =
    { trail_a_r_n : String.t option
    ; insight_selectors : InsightSelectors.t
    }

  let make ?trail_a_r_n ?(insight_selectors = []) () = { trail_a_r_n; insight_selectors }

  let parse xml =
    Some
      { trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
      ; insight_selectors =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "InsightSelectors" xml)
               InsightSelectors.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("InsightSelectors.member", InsightSelectors.to_query v.insight_selectors))
         ; Aws.Util.option_map v.trail_a_r_n (fun f ->
               Aws.Query.Pair ("TrailARN", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("InsightSelectors", InsightSelectors.to_json v.insight_selectors)
         ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
         ])

  let of_json j =
    { trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
    ; insight_selectors =
        InsightSelectors.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "InsightSelectors"))
    }
end

module DeleteTrailResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end
