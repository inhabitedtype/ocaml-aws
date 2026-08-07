open Aws.BaseTypes

type t =
  { creation_time : DateTime.t option
  ; deliver_logs_error_message : String.t option
  ; deliver_logs_permission_arn : String.t option
  ; deliver_cross_account_role : String.t option
  ; deliver_logs_status : String.t option
  ; flow_log_id : String.t option
  ; flow_log_status : String.t option
  ; log_group_name : String.t option
  ; resource_id : String.t option
  ; traffic_type : TrafficType.t option
  ; log_destination_type : LogDestinationType.t option
  ; log_destination : String.t option
  ; log_format : String.t option
  ; tags : TagList.t
  ; max_aggregation_interval : Integer.t option
  ; destination_options : DestinationOptionsResponse.t option
  ; tag_field_specifications : TagFieldSpecificationListResponse.t
  }

let make
    ?creation_time
    ?deliver_logs_error_message
    ?deliver_logs_permission_arn
    ?deliver_cross_account_role
    ?deliver_logs_status
    ?flow_log_id
    ?flow_log_status
    ?log_group_name
    ?resource_id
    ?traffic_type
    ?log_destination_type
    ?log_destination
    ?log_format
    ?(tags = [])
    ?max_aggregation_interval
    ?destination_options
    ?(tag_field_specifications = [])
    () =
  { creation_time
  ; deliver_logs_error_message
  ; deliver_logs_permission_arn
  ; deliver_cross_account_role
  ; deliver_logs_status
  ; flow_log_id
  ; flow_log_status
  ; log_group_name
  ; resource_id
  ; traffic_type
  ; log_destination_type
  ; log_destination
  ; log_format
  ; tags
  ; max_aggregation_interval
  ; destination_options
  ; tag_field_specifications
  }

let parse xml =
  Some
    { creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) DateTime.parse
    ; deliver_logs_error_message =
        Aws.Util.option_bind (Aws.Xml.member "deliverLogsErrorMessage" xml) String.parse
    ; deliver_logs_permission_arn =
        Aws.Util.option_bind (Aws.Xml.member "deliverLogsPermissionArn" xml) String.parse
    ; deliver_cross_account_role =
        Aws.Util.option_bind (Aws.Xml.member "deliverCrossAccountRole" xml) String.parse
    ; deliver_logs_status =
        Aws.Util.option_bind (Aws.Xml.member "deliverLogsStatus" xml) String.parse
    ; flow_log_id = Aws.Util.option_bind (Aws.Xml.member "flowLogId" xml) String.parse
    ; flow_log_status =
        Aws.Util.option_bind (Aws.Xml.member "flowLogStatus" xml) String.parse
    ; log_group_name =
        Aws.Util.option_bind (Aws.Xml.member "logGroupName" xml) String.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    ; traffic_type =
        Aws.Util.option_bind (Aws.Xml.member "trafficType" xml) TrafficType.parse
    ; log_destination_type =
        Aws.Util.option_bind
          (Aws.Xml.member "logDestinationType" xml)
          LogDestinationType.parse
    ; log_destination =
        Aws.Util.option_bind (Aws.Xml.member "logDestination" xml) String.parse
    ; log_format = Aws.Util.option_bind (Aws.Xml.member "logFormat" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; max_aggregation_interval =
        Aws.Util.option_bind (Aws.Xml.member "maxAggregationInterval" xml) Integer.parse
    ; destination_options =
        Aws.Util.option_bind
          (Aws.Xml.member "destinationOptions" xml)
          DestinationOptionsResponse.parse
    ; tag_field_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "tagFieldSpecificationSet" xml)
             TagFieldSpecificationListResponse.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "TagFieldSpecificationSet"
              , TagFieldSpecificationListResponse.to_query v.tag_field_specifications ))
       ; Aws.Util.option_map v.destination_options (fun f ->
             Aws.Query.Pair ("DestinationOptions", DestinationOptionsResponse.to_query f))
       ; Aws.Util.option_map v.max_aggregation_interval (fun f ->
             Aws.Query.Pair ("MaxAggregationInterval", Integer.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.log_format (fun f ->
             Aws.Query.Pair ("LogFormat", String.to_query f))
       ; Aws.Util.option_map v.log_destination (fun f ->
             Aws.Query.Pair ("LogDestination", String.to_query f))
       ; Aws.Util.option_map v.log_destination_type (fun f ->
             Aws.Query.Pair ("LogDestinationType", LogDestinationType.to_query f))
       ; Aws.Util.option_map v.traffic_type (fun f ->
             Aws.Query.Pair ("TrafficType", TrafficType.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.log_group_name (fun f ->
             Aws.Query.Pair ("LogGroupName", String.to_query f))
       ; Aws.Util.option_map v.flow_log_status (fun f ->
             Aws.Query.Pair ("FlowLogStatus", String.to_query f))
       ; Aws.Util.option_map v.flow_log_id (fun f ->
             Aws.Query.Pair ("FlowLogId", String.to_query f))
       ; Aws.Util.option_map v.deliver_logs_status (fun f ->
             Aws.Query.Pair ("DeliverLogsStatus", String.to_query f))
       ; Aws.Util.option_map v.deliver_cross_account_role (fun f ->
             Aws.Query.Pair ("DeliverCrossAccountRole", String.to_query f))
       ; Aws.Util.option_map v.deliver_logs_permission_arn (fun f ->
             Aws.Query.Pair ("DeliverLogsPermissionArn", String.to_query f))
       ; Aws.Util.option_map v.deliver_logs_error_message (fun f ->
             Aws.Query.Pair ("DeliverLogsErrorMessage", String.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "tagFieldSpecificationSet"
           , TagFieldSpecificationListResponse.to_json v.tag_field_specifications )
       ; Aws.Util.option_map v.destination_options (fun f ->
             "destinationOptions", DestinationOptionsResponse.to_json f)
       ; Aws.Util.option_map v.max_aggregation_interval (fun f ->
             "maxAggregationInterval", Integer.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.log_format (fun f -> "logFormat", String.to_json f)
       ; Aws.Util.option_map v.log_destination (fun f ->
             "logDestination", String.to_json f)
       ; Aws.Util.option_map v.log_destination_type (fun f ->
             "logDestinationType", LogDestinationType.to_json f)
       ; Aws.Util.option_map v.traffic_type (fun f ->
             "trafficType", TrafficType.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.log_group_name (fun f -> "logGroupName", String.to_json f)
       ; Aws.Util.option_map v.flow_log_status (fun f ->
             "flowLogStatus", String.to_json f)
       ; Aws.Util.option_map v.flow_log_id (fun f -> "flowLogId", String.to_json f)
       ; Aws.Util.option_map v.deliver_logs_status (fun f ->
             "deliverLogsStatus", String.to_json f)
       ; Aws.Util.option_map v.deliver_cross_account_role (fun f ->
             "deliverCrossAccountRole", String.to_json f)
       ; Aws.Util.option_map v.deliver_logs_permission_arn (fun f ->
             "deliverLogsPermissionArn", String.to_json f)
       ; Aws.Util.option_map v.deliver_logs_error_message (fun f ->
             "deliverLogsErrorMessage", String.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", DateTime.to_json f)
       ])

let of_json j =
  { creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "creationTime") DateTime.of_json
  ; deliver_logs_error_message =
      Aws.Util.option_map (Aws.Json.lookup j "deliverLogsErrorMessage") String.of_json
  ; deliver_logs_permission_arn =
      Aws.Util.option_map (Aws.Json.lookup j "deliverLogsPermissionArn") String.of_json
  ; deliver_cross_account_role =
      Aws.Util.option_map (Aws.Json.lookup j "deliverCrossAccountRole") String.of_json
  ; deliver_logs_status =
      Aws.Util.option_map (Aws.Json.lookup j "deliverLogsStatus") String.of_json
  ; flow_log_id = Aws.Util.option_map (Aws.Json.lookup j "flowLogId") String.of_json
  ; flow_log_status =
      Aws.Util.option_map (Aws.Json.lookup j "flowLogStatus") String.of_json
  ; log_group_name = Aws.Util.option_map (Aws.Json.lookup j "logGroupName") String.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  ; traffic_type =
      Aws.Util.option_map (Aws.Json.lookup j "trafficType") TrafficType.of_json
  ; log_destination_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "logDestinationType")
        LogDestinationType.of_json
  ; log_destination =
      Aws.Util.option_map (Aws.Json.lookup j "logDestination") String.of_json
  ; log_format = Aws.Util.option_map (Aws.Json.lookup j "logFormat") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; max_aggregation_interval =
      Aws.Util.option_map (Aws.Json.lookup j "maxAggregationInterval") Integer.of_json
  ; destination_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "destinationOptions")
        DestinationOptionsResponse.of_json
  ; tag_field_specifications =
      TagFieldSpecificationListResponse.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "tagFieldSpecificationSet"))
  }
