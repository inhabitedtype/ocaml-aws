open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; client_token : String.t option
  ; deliver_logs_permission_arn : String.t option
  ; deliver_cross_account_role : String.t option
  ; log_group_name : String.t option
  ; resource_ids : FlowLogResourceIds.t
  ; resource_type : FlowLogsResourceType.t
  ; traffic_type : TrafficType.t option
  ; log_destination_type : LogDestinationType.t option
  ; log_destination : String.t option
  ; log_format : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; max_aggregation_interval : Integer.t option
  ; destination_options : DestinationOptionsRequest.t option
  ; tag_field_specifications : TagFieldSpecificationListRequest.t
  }

let make
    ?dry_run
    ?client_token
    ?deliver_logs_permission_arn
    ?deliver_cross_account_role
    ?log_group_name
    ~resource_ids
    ~resource_type
    ?traffic_type
    ?log_destination_type
    ?log_destination
    ?log_format
    ?(tag_specifications = [])
    ?max_aggregation_interval
    ?destination_options
    ?(tag_field_specifications = [])
    () =
  { dry_run
  ; client_token
  ; deliver_logs_permission_arn
  ; deliver_cross_account_role
  ; log_group_name
  ; resource_ids
  ; resource_type
  ; traffic_type
  ; log_destination_type
  ; log_destination
  ; log_format
  ; tag_specifications
  ; max_aggregation_interval
  ; destination_options
  ; tag_field_specifications
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; deliver_logs_permission_arn =
        Aws.Util.option_bind (Aws.Xml.member "DeliverLogsPermissionArn" xml) String.parse
    ; deliver_cross_account_role =
        Aws.Util.option_bind (Aws.Xml.member "DeliverCrossAccountRole" xml) String.parse
    ; log_group_name =
        Aws.Util.option_bind (Aws.Xml.member "LogGroupName" xml) String.parse
    ; resource_ids =
        Aws.Xml.required
          "ResourceId"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceId" xml)
             FlowLogResourceIds.parse)
    ; resource_type =
        Aws.Xml.required
          "ResourceType"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceType" xml)
             FlowLogsResourceType.parse)
    ; traffic_type =
        Aws.Util.option_bind (Aws.Xml.member "TrafficType" xml) TrafficType.parse
    ; log_destination_type =
        Aws.Util.option_bind
          (Aws.Xml.member "LogDestinationType" xml)
          LogDestinationType.parse
    ; log_destination =
        Aws.Util.option_bind (Aws.Xml.member "LogDestination" xml) String.parse
    ; log_format = Aws.Util.option_bind (Aws.Xml.member "LogFormat" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; max_aggregation_interval =
        Aws.Util.option_bind (Aws.Xml.member "MaxAggregationInterval" xml) Integer.parse
    ; destination_options =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationOptions" xml)
          DestinationOptionsRequest.parse
    ; tag_field_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagFieldSpecification" xml)
             TagFieldSpecificationListRequest.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "TagFieldSpecification"
              , TagFieldSpecificationListRequest.to_query v.tag_field_specifications ))
       ; Aws.Util.option_map v.destination_options (fun f ->
             Aws.Query.Pair ("DestinationOptions", DestinationOptionsRequest.to_query f))
       ; Aws.Util.option_map v.max_aggregation_interval (fun f ->
             Aws.Query.Pair ("MaxAggregationInterval", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.log_format (fun f ->
             Aws.Query.Pair ("LogFormat", String.to_query f))
       ; Aws.Util.option_map v.log_destination (fun f ->
             Aws.Query.Pair ("LogDestination", String.to_query f))
       ; Aws.Util.option_map v.log_destination_type (fun f ->
             Aws.Query.Pair ("LogDestinationType", LogDestinationType.to_query f))
       ; Aws.Util.option_map v.traffic_type (fun f ->
             Aws.Query.Pair ("TrafficType", TrafficType.to_query f))
       ; Some
           (Aws.Query.Pair ("ResourceType", FlowLogsResourceType.to_query v.resource_type))
       ; Some (Aws.Query.Pair ("ResourceId", FlowLogResourceIds.to_query v.resource_ids))
       ; Aws.Util.option_map v.log_group_name (fun f ->
             Aws.Query.Pair ("LogGroupName", String.to_query f))
       ; Aws.Util.option_map v.deliver_cross_account_role (fun f ->
             Aws.Query.Pair ("DeliverCrossAccountRole", String.to_query f))
       ; Aws.Util.option_map v.deliver_logs_permission_arn (fun f ->
             Aws.Query.Pair ("DeliverLogsPermissionArn", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "TagFieldSpecification"
           , TagFieldSpecificationListRequest.to_json v.tag_field_specifications )
       ; Aws.Util.option_map v.destination_options (fun f ->
             "DestinationOptions", DestinationOptionsRequest.to_json f)
       ; Aws.Util.option_map v.max_aggregation_interval (fun f ->
             "MaxAggregationInterval", Integer.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.log_format (fun f -> "LogFormat", String.to_json f)
       ; Aws.Util.option_map v.log_destination (fun f ->
             "LogDestination", String.to_json f)
       ; Aws.Util.option_map v.log_destination_type (fun f ->
             "LogDestinationType", LogDestinationType.to_json f)
       ; Aws.Util.option_map v.traffic_type (fun f ->
             "TrafficType", TrafficType.to_json f)
       ; Some ("ResourceType", FlowLogsResourceType.to_json v.resource_type)
       ; Some ("ResourceId", FlowLogResourceIds.to_json v.resource_ids)
       ; Aws.Util.option_map v.log_group_name (fun f -> "LogGroupName", String.to_json f)
       ; Aws.Util.option_map v.deliver_cross_account_role (fun f ->
             "DeliverCrossAccountRole", String.to_json f)
       ; Aws.Util.option_map v.deliver_logs_permission_arn (fun f ->
             "DeliverLogsPermissionArn", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; deliver_logs_permission_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DeliverLogsPermissionArn") String.of_json
  ; deliver_cross_account_role =
      Aws.Util.option_map (Aws.Json.lookup j "DeliverCrossAccountRole") String.of_json
  ; log_group_name = Aws.Util.option_map (Aws.Json.lookup j "LogGroupName") String.of_json
  ; resource_ids =
      FlowLogResourceIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
  ; resource_type =
      FlowLogsResourceType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; traffic_type =
      Aws.Util.option_map (Aws.Json.lookup j "TrafficType") TrafficType.of_json
  ; log_destination_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "LogDestinationType")
        LogDestinationType.of_json
  ; log_destination =
      Aws.Util.option_map (Aws.Json.lookup j "LogDestination") String.of_json
  ; log_format = Aws.Util.option_map (Aws.Json.lookup j "LogFormat") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; max_aggregation_interval =
      Aws.Util.option_map (Aws.Json.lookup j "MaxAggregationInterval") Integer.of_json
  ; destination_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "DestinationOptions")
        DestinationOptionsRequest.of_json
  ; tag_field_specifications =
      TagFieldSpecificationListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagFieldSpecification"))
  }
