open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module Resource =
  struct
    type t = {
      resource_type: String.t option;
      resource_name: String.t option;}
    let make ?resource_type  ?resource_name  () =
      { resource_type; resource_name }
    let parse xml =
      Some
        {
          resource_type =
            (Util.option_bind (Xml.member "ResourceType" xml) String.parse);
          resource_name =
            (Util.option_bind (Xml.member "ResourceName" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.resource_name
              (fun f  -> Query.Pair ("ResourceName", (String.to_query f)));
           Util.option_map v.resource_type
             (fun f  -> Query.Pair ("ResourceType", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.resource_name
              (fun f  -> ("resource_name", (String.to_json f)));
           Util.option_map v.resource_type
             (fun f  -> ("resource_type", (String.to_json f)))])
    let of_json j =
      {
        resource_type =
          (Util.option_map (Json.lookup j "resource_type") String.of_json);
        resource_name =
          (Util.option_map (Json.lookup j "resource_name") String.of_json)
      }
  end
module LookupAttributeKey =
  struct
    type t =
      | EventId
      | EventName
      | Username
      | ResourceType
      | ResourceName
    let str_to_t =
      [("ResourceName", ResourceName);
      ("ResourceType", ResourceType);
      ("Username", Username);
      ("EventName", EventName);
      ("EventId", EventId)]
    let t_to_str =
      [(ResourceName, "ResourceName");
      (ResourceType, "ResourceType");
      (Username, "Username");
      (EventName, "EventName");
      (EventId, "EventId")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ResourceList =
  struct
    type t = Resource.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Resource.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Resource.to_query v
    let to_json v = `List (List.map Resource.to_json v)
    let of_json j = Json.to_list Resource.of_json j
  end
module LookupAttribute =
  struct
    type t = {
      attribute_key: LookupAttributeKey.t;
      attribute_value: String.t;}
    let make ~attribute_key  ~attribute_value  () =
      { attribute_key; attribute_value }
    let parse xml =
      Some
        {
          attribute_key =
            (Xml.required "AttributeKey"
               (Util.option_bind (Xml.member "AttributeKey" xml)
                  LookupAttributeKey.parse));
          attribute_value =
            (Xml.required "AttributeValue"
               (Util.option_bind (Xml.member "AttributeValue" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AttributeValue", (String.to_query v.attribute_value)));
           Some
             (Query.Pair
                ("AttributeKey",
                  (LookupAttributeKey.to_query v.attribute_key)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("attribute_value", (String.to_json v.attribute_value));
           Some
             ("attribute_key", (LookupAttributeKey.to_json v.attribute_key))])
    let of_json j =
      {
        attribute_key =
          (LookupAttributeKey.of_json
             (Util.of_option_exn (Json.lookup j "attribute_key")));
        attribute_value =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "attribute_value")))
      }
  end
module Event =
  struct
    type t =
      {
      event_id: String.t option;
      event_name: String.t option;
      event_time: DateTime.t option;
      username: String.t option;
      resources: ResourceList.t;
      cloud_trail_event: String.t option;}
    let make ?event_id  ?event_name  ?event_time  ?username  ?(resources= [])
       ?cloud_trail_event  () =
      {
        event_id;
        event_name;
        event_time;
        username;
        resources;
        cloud_trail_event
      }
    let parse xml =
      Some
        {
          event_id =
            (Util.option_bind (Xml.member "EventId" xml) String.parse);
          event_name =
            (Util.option_bind (Xml.member "EventName" xml) String.parse);
          event_time =
            (Util.option_bind (Xml.member "EventTime" xml) DateTime.parse);
          username =
            (Util.option_bind (Xml.member "Username" xml) String.parse);
          resources =
            (Util.of_option []
               (Util.option_bind (Xml.member "Resources" xml)
                  ResourceList.parse));
          cloud_trail_event =
            (Util.option_bind (Xml.member "CloudTrailEvent" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cloud_trail_event
              (fun f  -> Query.Pair ("CloudTrailEvent", (String.to_query f)));
           Some
             (Query.Pair
                ("Resources.member", (ResourceList.to_query v.resources)));
           Util.option_map v.username
             (fun f  -> Query.Pair ("Username", (String.to_query f)));
           Util.option_map v.event_time
             (fun f  -> Query.Pair ("EventTime", (DateTime.to_query f)));
           Util.option_map v.event_name
             (fun f  -> Query.Pair ("EventName", (String.to_query f)));
           Util.option_map v.event_id
             (fun f  -> Query.Pair ("EventId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cloud_trail_event
              (fun f  -> ("cloud_trail_event", (String.to_json f)));
           Some ("resources", (ResourceList.to_json v.resources));
           Util.option_map v.username
             (fun f  -> ("username", (String.to_json f)));
           Util.option_map v.event_time
             (fun f  -> ("event_time", (DateTime.to_json f)));
           Util.option_map v.event_name
             (fun f  -> ("event_name", (String.to_json f)));
           Util.option_map v.event_id
             (fun f  -> ("event_id", (String.to_json f)))])
    let of_json j =
      {
        event_id =
          (Util.option_map (Json.lookup j "event_id") String.of_json);
        event_name =
          (Util.option_map (Json.lookup j "event_name") String.of_json);
        event_time =
          (Util.option_map (Json.lookup j "event_time") DateTime.of_json);
        username =
          (Util.option_map (Json.lookup j "username") String.of_json);
        resources =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "resources")));
        cloud_trail_event =
          (Util.option_map (Json.lookup j "cloud_trail_event") String.of_json)
      }
  end
module Trail =
  struct
    type t =
      {
      name: String.t option;
      s3_bucket_name: String.t option;
      s3_key_prefix: String.t option;
      sns_topic_name: String.t option;
      include_global_service_events: Boolean.t option;
      cloud_watch_logs_log_group_arn: String.t option;
      cloud_watch_logs_role_arn: String.t option;}
    let make ?name  ?s3_bucket_name  ?s3_key_prefix  ?sns_topic_name 
      ?include_global_service_events  ?cloud_watch_logs_log_group_arn 
      ?cloud_watch_logs_role_arn  () =
      {
        name;
        s3_bucket_name;
        s3_key_prefix;
        sns_topic_name;
        include_global_service_events;
        cloud_watch_logs_log_group_arn;
        cloud_watch_logs_role_arn
      }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          s3_bucket_name =
            (Util.option_bind (Xml.member "S3BucketName" xml) String.parse);
          s3_key_prefix =
            (Util.option_bind (Xml.member "S3KeyPrefix" xml) String.parse);
          sns_topic_name =
            (Util.option_bind (Xml.member "SnsTopicName" xml) String.parse);
          include_global_service_events =
            (Util.option_bind (Xml.member "IncludeGlobalServiceEvents" xml)
               Boolean.parse);
          cloud_watch_logs_log_group_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse);
          cloud_watch_logs_role_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsRoleArn" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_logs_role_arn
              (fun f  ->
                 Query.Pair ("CloudWatchLogsRoleArn", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f  ->
                Query.Pair ("CloudWatchLogsLogGroupArn", (String.to_query f)));
           Util.option_map v.include_global_service_events
             (fun f  ->
                Query.Pair
                  ("IncludeGlobalServiceEvents", (Boolean.to_query f)));
           Util.option_map v.sns_topic_name
             (fun f  -> Query.Pair ("SnsTopicName", (String.to_query f)));
           Util.option_map v.s3_key_prefix
             (fun f  -> Query.Pair ("S3KeyPrefix", (String.to_query f)));
           Util.option_map v.s3_bucket_name
             (fun f  -> Query.Pair ("S3BucketName", (String.to_query f)));
           Util.option_map v.name
             (fun f  -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_logs_role_arn
              (fun f  -> ("cloud_watch_logs_role_arn", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f  ->
                ("cloud_watch_logs_log_group_arn", (String.to_json f)));
           Util.option_map v.include_global_service_events
             (fun f  ->
                ("include_global_service_events", (Boolean.to_json f)));
           Util.option_map v.sns_topic_name
             (fun f  -> ("sns_topic_name", (String.to_json f)));
           Util.option_map v.s3_key_prefix
             (fun f  -> ("s3_key_prefix", (String.to_json f)));
           Util.option_map v.s3_bucket_name
             (fun f  -> ("s3_bucket_name", (String.to_json f)));
           Util.option_map v.name (fun f  -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        s3_bucket_name =
          (Util.option_map (Json.lookup j "s3_bucket_name") String.of_json);
        s3_key_prefix =
          (Util.option_map (Json.lookup j "s3_key_prefix") String.of_json);
        sns_topic_name =
          (Util.option_map (Json.lookup j "sns_topic_name") String.of_json);
        include_global_service_events =
          (Util.option_map (Json.lookup j "include_global_service_events")
             Boolean.of_json);
        cloud_watch_logs_log_group_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_log_group_arn")
             String.of_json);
        cloud_watch_logs_role_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_role_arn")
             String.of_json)
      }
  end
module LookupAttributesList =
  struct
    type t = LookupAttribute.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map LookupAttribute.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list LookupAttribute.to_query v
    let to_json v = `List (List.map LookupAttribute.to_json v)
    let of_json j = Json.to_list LookupAttribute.of_json j
  end
module TrailNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module EventsList =
  struct
    type t = Event.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Event.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Event.to_query v
    let to_json v = `List (List.map Event.to_json v)
    let of_json j = Json.to_list Event.of_json j
  end
module TrailList =
  struct
    type t = Trail.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Trail.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Trail.to_query v
    let to_json v = `List (List.map Trail.to_json v)
    let of_json j = Json.to_list Trail.of_json j
  end
module StopLoggingRequest =
  struct
    type t = {
      name: String.t;}
    let make ~name  () = { name }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("name", (String.to_json v.name))])
    let of_json j =
      { name = (String.of_json (Util.of_option_exn (Json.lookup j "name"))) }
  end
module StartLoggingRequest =
  struct
    type t = {
      name: String.t;}
    let make ~name  () = { name }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("name", (String.to_json v.name))])
    let of_json j =
      { name = (String.of_json (Util.of_option_exn (Json.lookup j "name"))) }
  end
module LookupEventsRequest =
  struct
    type t =
      {
      lookup_attributes: LookupAttributesList.t;
      start_time: DateTime.t option;
      end_time: DateTime.t option;
      max_results: Integer.t option;
      next_token: String.t option;}
    let make ?(lookup_attributes= [])  ?start_time  ?end_time  ?max_results 
      ?next_token  () =
      { lookup_attributes; start_time; end_time; max_results; next_token }
    let parse xml =
      Some
        {
          lookup_attributes =
            (Util.of_option []
               (Util.option_bind (Xml.member "LookupAttributes" xml)
                  LookupAttributesList.parse));
          start_time =
            (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse);
          end_time =
            (Util.option_bind (Xml.member "EndTime" xml) DateTime.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f  -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f  -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.end_time
             (fun f  -> Query.Pair ("EndTime", (DateTime.to_query f)));
           Util.option_map v.start_time
             (fun f  -> Query.Pair ("StartTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("LookupAttributes.member",
                  (LookupAttributesList.to_query v.lookup_attributes)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f  -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f  -> ("max_results", (Integer.to_json f)));
           Util.option_map v.end_time
             (fun f  -> ("end_time", (DateTime.to_json f)));
           Util.option_map v.start_time
             (fun f  -> ("start_time", (DateTime.to_json f)));
           Some
             ("lookup_attributes",
               (LookupAttributesList.to_json v.lookup_attributes))])
    let of_json j =
      {
        lookup_attributes =
          (LookupAttributesList.of_json
             (Util.of_option_exn (Json.lookup j "lookup_attributes")));
        start_time =
          (Util.option_map (Json.lookup j "start_time") DateTime.of_json);
        end_time =
          (Util.option_map (Json.lookup j "end_time") DateTime.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module GetTrailStatusResponse =
  struct
    type t =
      {
      is_logging: Boolean.t option;
      latest_delivery_error: String.t option;
      latest_notification_error: String.t option;
      latest_delivery_time: DateTime.t option;
      latest_notification_time: DateTime.t option;
      start_logging_time: DateTime.t option;
      stop_logging_time: DateTime.t option;
      latest_cloud_watch_logs_delivery_error: String.t option;
      latest_cloud_watch_logs_delivery_time: DateTime.t option;}
    let make ?is_logging  ?latest_delivery_error  ?latest_notification_error 
      ?latest_delivery_time  ?latest_notification_time  ?start_logging_time 
      ?stop_logging_time  ?latest_cloud_watch_logs_delivery_error 
      ?latest_cloud_watch_logs_delivery_time  () =
      {
        is_logging;
        latest_delivery_error;
        latest_notification_error;
        latest_delivery_time;
        latest_notification_time;
        start_logging_time;
        stop_logging_time;
        latest_cloud_watch_logs_delivery_error;
        latest_cloud_watch_logs_delivery_time
      }
    let parse xml =
      Some
        {
          is_logging =
            (Util.option_bind (Xml.member "IsLogging" xml) Boolean.parse);
          latest_delivery_error =
            (Util.option_bind (Xml.member "LatestDeliveryError" xml)
               String.parse);
          latest_notification_error =
            (Util.option_bind (Xml.member "LatestNotificationError" xml)
               String.parse);
          latest_delivery_time =
            (Util.option_bind (Xml.member "LatestDeliveryTime" xml)
               DateTime.parse);
          latest_notification_time =
            (Util.option_bind (Xml.member "LatestNotificationTime" xml)
               DateTime.parse);
          start_logging_time =
            (Util.option_bind (Xml.member "StartLoggingTime" xml)
               DateTime.parse);
          stop_logging_time =
            (Util.option_bind (Xml.member "StopLoggingTime" xml)
               DateTime.parse);
          latest_cloud_watch_logs_delivery_error =
            (Util.option_bind
               (Xml.member "LatestCloudWatchLogsDeliveryError" xml)
               String.parse);
          latest_cloud_watch_logs_delivery_time =
            (Util.option_bind
               (Xml.member "LatestCloudWatchLogsDeliveryTime" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.latest_cloud_watch_logs_delivery_time
              (fun f  ->
                 Query.Pair
                   ("LatestCloudWatchLogsDeliveryTime",
                     (DateTime.to_query f)));
           Util.option_map v.latest_cloud_watch_logs_delivery_error
             (fun f  ->
                Query.Pair
                  ("LatestCloudWatchLogsDeliveryError", (String.to_query f)));
           Util.option_map v.stop_logging_time
             (fun f  -> Query.Pair ("StopLoggingTime", (DateTime.to_query f)));
           Util.option_map v.start_logging_time
             (fun f  ->
                Query.Pair ("StartLoggingTime", (DateTime.to_query f)));
           Util.option_map v.latest_notification_time
             (fun f  ->
                Query.Pair ("LatestNotificationTime", (DateTime.to_query f)));
           Util.option_map v.latest_delivery_time
             (fun f  ->
                Query.Pair ("LatestDeliveryTime", (DateTime.to_query f)));
           Util.option_map v.latest_notification_error
             (fun f  ->
                Query.Pair ("LatestNotificationError", (String.to_query f)));
           Util.option_map v.latest_delivery_error
             (fun f  ->
                Query.Pair ("LatestDeliveryError", (String.to_query f)));
           Util.option_map v.is_logging
             (fun f  -> Query.Pair ("IsLogging", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.latest_cloud_watch_logs_delivery_time
              (fun f  ->
                 ("latest_cloud_watch_logs_delivery_time",
                   (DateTime.to_json f)));
           Util.option_map v.latest_cloud_watch_logs_delivery_error
             (fun f  ->
                ("latest_cloud_watch_logs_delivery_error",
                  (String.to_json f)));
           Util.option_map v.stop_logging_time
             (fun f  -> ("stop_logging_time", (DateTime.to_json f)));
           Util.option_map v.start_logging_time
             (fun f  -> ("start_logging_time", (DateTime.to_json f)));
           Util.option_map v.latest_notification_time
             (fun f  -> ("latest_notification_time", (DateTime.to_json f)));
           Util.option_map v.latest_delivery_time
             (fun f  -> ("latest_delivery_time", (DateTime.to_json f)));
           Util.option_map v.latest_notification_error
             (fun f  -> ("latest_notification_error", (String.to_json f)));
           Util.option_map v.latest_delivery_error
             (fun f  -> ("latest_delivery_error", (String.to_json f)));
           Util.option_map v.is_logging
             (fun f  -> ("is_logging", (Boolean.to_json f)))])
    let of_json j =
      {
        is_logging =
          (Util.option_map (Json.lookup j "is_logging") Boolean.of_json);
        latest_delivery_error =
          (Util.option_map (Json.lookup j "latest_delivery_error")
             String.of_json);
        latest_notification_error =
          (Util.option_map (Json.lookup j "latest_notification_error")
             String.of_json);
        latest_delivery_time =
          (Util.option_map (Json.lookup j "latest_delivery_time")
             DateTime.of_json);
        latest_notification_time =
          (Util.option_map (Json.lookup j "latest_notification_time")
             DateTime.of_json);
        start_logging_time =
          (Util.option_map (Json.lookup j "start_logging_time")
             DateTime.of_json);
        stop_logging_time =
          (Util.option_map (Json.lookup j "stop_logging_time")
             DateTime.of_json);
        latest_cloud_watch_logs_delivery_error =
          (Util.option_map
             (Json.lookup j "latest_cloud_watch_logs_delivery_error")
             String.of_json);
        latest_cloud_watch_logs_delivery_time =
          (Util.option_map
             (Json.lookup j "latest_cloud_watch_logs_delivery_time")
             DateTime.of_json)
      }
  end
module DeleteTrailRequest =
  struct
    type t = {
      name: String.t;}
    let make ~name  () = { name }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("name", (String.to_json v.name))])
    let of_json j =
      { name = (String.of_json (Util.of_option_exn (Json.lookup j "name"))) }
  end
module UpdateTrailResponse =
  struct
    type t =
      {
      name: String.t option;
      s3_bucket_name: String.t option;
      s3_key_prefix: String.t option;
      sns_topic_name: String.t option;
      include_global_service_events: Boolean.t option;
      cloud_watch_logs_log_group_arn: String.t option;
      cloud_watch_logs_role_arn: String.t option;}
    let make ?name  ?s3_bucket_name  ?s3_key_prefix  ?sns_topic_name 
      ?include_global_service_events  ?cloud_watch_logs_log_group_arn 
      ?cloud_watch_logs_role_arn  () =
      {
        name;
        s3_bucket_name;
        s3_key_prefix;
        sns_topic_name;
        include_global_service_events;
        cloud_watch_logs_log_group_arn;
        cloud_watch_logs_role_arn
      }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          s3_bucket_name =
            (Util.option_bind (Xml.member "S3BucketName" xml) String.parse);
          s3_key_prefix =
            (Util.option_bind (Xml.member "S3KeyPrefix" xml) String.parse);
          sns_topic_name =
            (Util.option_bind (Xml.member "SnsTopicName" xml) String.parse);
          include_global_service_events =
            (Util.option_bind (Xml.member "IncludeGlobalServiceEvents" xml)
               Boolean.parse);
          cloud_watch_logs_log_group_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse);
          cloud_watch_logs_role_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsRoleArn" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_logs_role_arn
              (fun f  ->
                 Query.Pair ("CloudWatchLogsRoleArn", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f  ->
                Query.Pair ("CloudWatchLogsLogGroupArn", (String.to_query f)));
           Util.option_map v.include_global_service_events
             (fun f  ->
                Query.Pair
                  ("IncludeGlobalServiceEvents", (Boolean.to_query f)));
           Util.option_map v.sns_topic_name
             (fun f  -> Query.Pair ("SnsTopicName", (String.to_query f)));
           Util.option_map v.s3_key_prefix
             (fun f  -> Query.Pair ("S3KeyPrefix", (String.to_query f)));
           Util.option_map v.s3_bucket_name
             (fun f  -> Query.Pair ("S3BucketName", (String.to_query f)));
           Util.option_map v.name
             (fun f  -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_logs_role_arn
              (fun f  -> ("cloud_watch_logs_role_arn", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f  ->
                ("cloud_watch_logs_log_group_arn", (String.to_json f)));
           Util.option_map v.include_global_service_events
             (fun f  ->
                ("include_global_service_events", (Boolean.to_json f)));
           Util.option_map v.sns_topic_name
             (fun f  -> ("sns_topic_name", (String.to_json f)));
           Util.option_map v.s3_key_prefix
             (fun f  -> ("s3_key_prefix", (String.to_json f)));
           Util.option_map v.s3_bucket_name
             (fun f  -> ("s3_bucket_name", (String.to_json f)));
           Util.option_map v.name (fun f  -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        s3_bucket_name =
          (Util.option_map (Json.lookup j "s3_bucket_name") String.of_json);
        s3_key_prefix =
          (Util.option_map (Json.lookup j "s3_key_prefix") String.of_json);
        sns_topic_name =
          (Util.option_map (Json.lookup j "sns_topic_name") String.of_json);
        include_global_service_events =
          (Util.option_map (Json.lookup j "include_global_service_events")
             Boolean.of_json);
        cloud_watch_logs_log_group_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_log_group_arn")
             String.of_json);
        cloud_watch_logs_role_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_role_arn")
             String.of_json)
      }
  end
module CreateTrailRequest =
  struct
    type t =
      {
      name: String.t;
      s3_bucket_name: String.t;
      s3_key_prefix: String.t option;
      sns_topic_name: String.t option;
      include_global_service_events: Boolean.t option;
      cloud_watch_logs_log_group_arn: String.t option;
      cloud_watch_logs_role_arn: String.t option;}
    let make ~name  ~s3_bucket_name  ?s3_key_prefix  ?sns_topic_name 
      ?include_global_service_events  ?cloud_watch_logs_log_group_arn 
      ?cloud_watch_logs_role_arn  () =
      {
        name;
        s3_bucket_name;
        s3_key_prefix;
        sns_topic_name;
        include_global_service_events;
        cloud_watch_logs_log_group_arn;
        cloud_watch_logs_role_arn
      }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          s3_bucket_name =
            (Xml.required "S3BucketName"
               (Util.option_bind (Xml.member "S3BucketName" xml) String.parse));
          s3_key_prefix =
            (Util.option_bind (Xml.member "S3KeyPrefix" xml) String.parse);
          sns_topic_name =
            (Util.option_bind (Xml.member "SnsTopicName" xml) String.parse);
          include_global_service_events =
            (Util.option_bind (Xml.member "IncludeGlobalServiceEvents" xml)
               Boolean.parse);
          cloud_watch_logs_log_group_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse);
          cloud_watch_logs_role_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsRoleArn" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_logs_role_arn
              (fun f  ->
                 Query.Pair ("CloudWatchLogsRoleArn", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f  ->
                Query.Pair ("CloudWatchLogsLogGroupArn", (String.to_query f)));
           Util.option_map v.include_global_service_events
             (fun f  ->
                Query.Pair
                  ("IncludeGlobalServiceEvents", (Boolean.to_query f)));
           Util.option_map v.sns_topic_name
             (fun f  -> Query.Pair ("SnsTopicName", (String.to_query f)));
           Util.option_map v.s3_key_prefix
             (fun f  -> Query.Pair ("S3KeyPrefix", (String.to_query f)));
           Some
             (Query.Pair ("S3BucketName", (String.to_query v.s3_bucket_name)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_logs_role_arn
              (fun f  -> ("cloud_watch_logs_role_arn", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f  ->
                ("cloud_watch_logs_log_group_arn", (String.to_json f)));
           Util.option_map v.include_global_service_events
             (fun f  ->
                ("include_global_service_events", (Boolean.to_json f)));
           Util.option_map v.sns_topic_name
             (fun f  -> ("sns_topic_name", (String.to_json f)));
           Util.option_map v.s3_key_prefix
             (fun f  -> ("s3_key_prefix", (String.to_json f)));
           Some ("s3_bucket_name", (String.to_json v.s3_bucket_name));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        s3_bucket_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "s3_bucket_name")));
        s3_key_prefix =
          (Util.option_map (Json.lookup j "s3_key_prefix") String.of_json);
        sns_topic_name =
          (Util.option_map (Json.lookup j "sns_topic_name") String.of_json);
        include_global_service_events =
          (Util.option_map (Json.lookup j "include_global_service_events")
             Boolean.of_json);
        cloud_watch_logs_log_group_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_log_group_arn")
             String.of_json);
        cloud_watch_logs_role_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_role_arn")
             String.of_json)
      }
  end
module GetTrailStatusRequest =
  struct
    type t = {
      name: String.t;}
    let make ~name  () = { name }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("name", (String.to_json v.name))])
    let of_json j =
      { name = (String.of_json (Util.of_option_exn (Json.lookup j "name"))) }
  end
module UpdateTrailRequest =
  struct
    type t =
      {
      name: String.t;
      s3_bucket_name: String.t option;
      s3_key_prefix: String.t option;
      sns_topic_name: String.t option;
      include_global_service_events: Boolean.t option;
      cloud_watch_logs_log_group_arn: String.t option;
      cloud_watch_logs_role_arn: String.t option;}
    let make ~name  ?s3_bucket_name  ?s3_key_prefix  ?sns_topic_name 
      ?include_global_service_events  ?cloud_watch_logs_log_group_arn 
      ?cloud_watch_logs_role_arn  () =
      {
        name;
        s3_bucket_name;
        s3_key_prefix;
        sns_topic_name;
        include_global_service_events;
        cloud_watch_logs_log_group_arn;
        cloud_watch_logs_role_arn
      }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          s3_bucket_name =
            (Util.option_bind (Xml.member "S3BucketName" xml) String.parse);
          s3_key_prefix =
            (Util.option_bind (Xml.member "S3KeyPrefix" xml) String.parse);
          sns_topic_name =
            (Util.option_bind (Xml.member "SnsTopicName" xml) String.parse);
          include_global_service_events =
            (Util.option_bind (Xml.member "IncludeGlobalServiceEvents" xml)
               Boolean.parse);
          cloud_watch_logs_log_group_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse);
          cloud_watch_logs_role_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsRoleArn" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_logs_role_arn
              (fun f  ->
                 Query.Pair ("CloudWatchLogsRoleArn", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f  ->
                Query.Pair ("CloudWatchLogsLogGroupArn", (String.to_query f)));
           Util.option_map v.include_global_service_events
             (fun f  ->
                Query.Pair
                  ("IncludeGlobalServiceEvents", (Boolean.to_query f)));
           Util.option_map v.sns_topic_name
             (fun f  -> Query.Pair ("SnsTopicName", (String.to_query f)));
           Util.option_map v.s3_key_prefix
             (fun f  -> Query.Pair ("S3KeyPrefix", (String.to_query f)));
           Util.option_map v.s3_bucket_name
             (fun f  -> Query.Pair ("S3BucketName", (String.to_query f)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_logs_role_arn
              (fun f  -> ("cloud_watch_logs_role_arn", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f  ->
                ("cloud_watch_logs_log_group_arn", (String.to_json f)));
           Util.option_map v.include_global_service_events
             (fun f  ->
                ("include_global_service_events", (Boolean.to_json f)));
           Util.option_map v.sns_topic_name
             (fun f  -> ("sns_topic_name", (String.to_json f)));
           Util.option_map v.s3_key_prefix
             (fun f  -> ("s3_key_prefix", (String.to_json f)));
           Util.option_map v.s3_bucket_name
             (fun f  -> ("s3_bucket_name", (String.to_json f)));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        s3_bucket_name =
          (Util.option_map (Json.lookup j "s3_bucket_name") String.of_json);
        s3_key_prefix =
          (Util.option_map (Json.lookup j "s3_key_prefix") String.of_json);
        sns_topic_name =
          (Util.option_map (Json.lookup j "sns_topic_name") String.of_json);
        include_global_service_events =
          (Util.option_map (Json.lookup j "include_global_service_events")
             Boolean.of_json);
        cloud_watch_logs_log_group_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_log_group_arn")
             String.of_json);
        cloud_watch_logs_role_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_role_arn")
             String.of_json)
      }
  end
module DescribeTrailsRequest =
  struct
    type t = {
      trail_name_list: TrailNameList.t;}
    let make ?(trail_name_list= [])  () = { trail_name_list }
    let parse xml =
      Some
        {
          trail_name_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "trailNameList" xml)
                  TrailNameList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("trailNameList.member",
                   (TrailNameList.to_query v.trail_name_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("trail_name_list", (TrailNameList.to_json v.trail_name_list))])
    let of_json j =
      {
        trail_name_list =
          (TrailNameList.of_json
             (Util.of_option_exn (Json.lookup j "trail_name_list")))
      }
  end
module LookupEventsResponse =
  struct
    type t = {
      events: EventsList.t;
      next_token: String.t option;}
    let make ?(events= [])  ?next_token  () = { events; next_token }
    let parse xml =
      Some
        {
          events =
            (Util.of_option []
               (Util.option_bind (Xml.member "Events" xml) EventsList.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f  -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair ("Events.member", (EventsList.to_query v.events)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f  -> ("next_token", (String.to_json f)));
           Some ("events", (EventsList.to_json v.events))])
    let of_json j =
      {
        events =
          (EventsList.of_json (Util.of_option_exn (Json.lookup j "events")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module CreateTrailResponse =
  struct
    type t =
      {
      name: String.t option;
      s3_bucket_name: String.t option;
      s3_key_prefix: String.t option;
      sns_topic_name: String.t option;
      include_global_service_events: Boolean.t option;
      cloud_watch_logs_log_group_arn: String.t option;
      cloud_watch_logs_role_arn: String.t option;}
    let make ?name  ?s3_bucket_name  ?s3_key_prefix  ?sns_topic_name 
      ?include_global_service_events  ?cloud_watch_logs_log_group_arn 
      ?cloud_watch_logs_role_arn  () =
      {
        name;
        s3_bucket_name;
        s3_key_prefix;
        sns_topic_name;
        include_global_service_events;
        cloud_watch_logs_log_group_arn;
        cloud_watch_logs_role_arn
      }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          s3_bucket_name =
            (Util.option_bind (Xml.member "S3BucketName" xml) String.parse);
          s3_key_prefix =
            (Util.option_bind (Xml.member "S3KeyPrefix" xml) String.parse);
          sns_topic_name =
            (Util.option_bind (Xml.member "SnsTopicName" xml) String.parse);
          include_global_service_events =
            (Util.option_bind (Xml.member "IncludeGlobalServiceEvents" xml)
               Boolean.parse);
          cloud_watch_logs_log_group_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse);
          cloud_watch_logs_role_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsRoleArn" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_logs_role_arn
              (fun f  ->
                 Query.Pair ("CloudWatchLogsRoleArn", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f  ->
                Query.Pair ("CloudWatchLogsLogGroupArn", (String.to_query f)));
           Util.option_map v.include_global_service_events
             (fun f  ->
                Query.Pair
                  ("IncludeGlobalServiceEvents", (Boolean.to_query f)));
           Util.option_map v.sns_topic_name
             (fun f  -> Query.Pair ("SnsTopicName", (String.to_query f)));
           Util.option_map v.s3_key_prefix
             (fun f  -> Query.Pair ("S3KeyPrefix", (String.to_query f)));
           Util.option_map v.s3_bucket_name
             (fun f  -> Query.Pair ("S3BucketName", (String.to_query f)));
           Util.option_map v.name
             (fun f  -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_logs_role_arn
              (fun f  -> ("cloud_watch_logs_role_arn", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f  ->
                ("cloud_watch_logs_log_group_arn", (String.to_json f)));
           Util.option_map v.include_global_service_events
             (fun f  ->
                ("include_global_service_events", (Boolean.to_json f)));
           Util.option_map v.sns_topic_name
             (fun f  -> ("sns_topic_name", (String.to_json f)));
           Util.option_map v.s3_key_prefix
             (fun f  -> ("s3_key_prefix", (String.to_json f)));
           Util.option_map v.s3_bucket_name
             (fun f  -> ("s3_bucket_name", (String.to_json f)));
           Util.option_map v.name (fun f  -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        s3_bucket_name =
          (Util.option_map (Json.lookup j "s3_bucket_name") String.of_json);
        s3_key_prefix =
          (Util.option_map (Json.lookup j "s3_key_prefix") String.of_json);
        sns_topic_name =
          (Util.option_map (Json.lookup j "sns_topic_name") String.of_json);
        include_global_service_events =
          (Util.option_map (Json.lookup j "include_global_service_events")
             Boolean.of_json);
        cloud_watch_logs_log_group_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_log_group_arn")
             String.of_json);
        cloud_watch_logs_role_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_role_arn")
             String.of_json)
      }
  end
module DescribeTrailsResponse =
  struct
    type t = {
      trail_list: TrailList.t;}
    let make ?(trail_list= [])  () = { trail_list }
    let parse xml =
      Some
        {
          trail_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "trailList" xml) TrailList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("trailList.member", (TrailList.to_query v.trail_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("trail_list", (TrailList.to_json v.trail_list))])
    let of_json j =
      {
        trail_list =
          (TrailList.of_json
             (Util.of_option_exn (Json.lookup j "trail_list")))
      }
  end