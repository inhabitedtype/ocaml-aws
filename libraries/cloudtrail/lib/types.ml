open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module DataResourceValues =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Tag =
  struct
    type t = {
      key: String.t ;
      value: String.t option }
    let make ~key  ?value  () = { key; value }
    let parse xml =
      Some
        {
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          value = (Util.option_bind (Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f -> Query.Pair ("Value", (String.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Some ("key", (String.to_json v.key))])
    let of_json j =
      {
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      }
  end
module DataResource =
  struct
    type t = {
      type_: String.t option ;
      values: DataResourceValues.t }
    let make ?type_  ?(values= [])  () = { type_; values }
    let parse xml =
      Some
        {
          type_ = (Util.option_bind (Xml.member "Type" xml) String.parse);
          values =
            (Util.of_option []
               (Util.option_bind (Xml.member "Values" xml)
                  DataResourceValues.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Values.member", (DataResourceValues.to_query v.values)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("values", (DataResourceValues.to_json v.values));
           Util.option_map v.type_ (fun f -> ("type_", (String.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") String.of_json);
        values =
          (DataResourceValues.of_json
             (Util.of_option_exn (Json.lookup j "values")))
      }
  end
module Resource =
  struct
    type t =
      {
      resource_type: String.t option ;
      resource_name: String.t option }
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
              (fun f -> Query.Pair ("ResourceName", (String.to_query f)));
           Util.option_map v.resource_type
             (fun f -> Query.Pair ("ResourceType", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.resource_name
              (fun f -> ("resource_name", (String.to_json f)));
           Util.option_map v.resource_type
             (fun f -> ("resource_type", (String.to_json f)))])
    let of_json j =
      {
        resource_type =
          (Util.option_map (Json.lookup j "resource_type") String.of_json);
        resource_name =
          (Util.option_map (Json.lookup j "resource_name") String.of_json)
      }
  end
module InsightType =
  struct
    type t =
      | ApiCallRateInsight 
    let str_to_t = [("ApiCallRateInsight", ApiCallRateInsight)]
    let t_to_str = [(ApiCallRateInsight, "ApiCallRateInsight")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module TagsList =
  struct
    type t = Tag.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Tag.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Tag.to_query v
    let to_json v = `List (List.map Tag.to_json v)
    let of_json j = Json.to_list Tag.of_json j
  end
module DataResources =
  struct
    type t = DataResource.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DataResource.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list DataResource.to_query v
    let to_json v = `List (List.map DataResource.to_json v)
    let of_json j = Json.to_list DataResource.of_json j
  end
module ExcludeManagementEventSources =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ReadWriteType =
  struct
    type t =
      | ReadOnly 
      | WriteOnly 
      | All 
    let str_to_t =
      [("All", All); ("WriteOnly", WriteOnly); ("ReadOnly", ReadOnly)]
    let t_to_str =
      [(All, "All"); (WriteOnly, "WriteOnly"); (ReadOnly, "ReadOnly")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module LookupAttributeKey =
  struct
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
      [("AccessKeyId", AccessKeyId);
      ("EventSource", EventSource);
      ("ResourceName", ResourceName);
      ("ResourceType", ResourceType);
      ("Username", Username);
      ("ReadOnly", ReadOnly);
      ("EventName", EventName);
      ("EventId", EventId)]
    let t_to_str =
      [(AccessKeyId, "AccessKeyId");
      (EventSource, "EventSource");
      (ResourceName, "ResourceName");
      (ResourceType, "ResourceType");
      (Username, "Username");
      (ReadOnly, "ReadOnly");
      (EventName, "EventName");
      (EventId, "EventId")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
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
module InsightSelector =
  struct
    type t = {
      insight_type: InsightType.t option }
    let make ?insight_type  () = { insight_type }
    let parse xml =
      Some
        {
          insight_type =
            (Util.option_bind (Xml.member "InsightType" xml)
               InsightType.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.insight_type
              (fun f -> Query.Pair ("InsightType", (InsightType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.insight_type
              (fun f -> ("insight_type", (InsightType.to_json f)))])
    let of_json j =
      {
        insight_type =
          (Util.option_map (Json.lookup j "insight_type") InsightType.of_json)
      }
  end
module ResourceTag =
  struct
    type t = {
      resource_id: String.t option ;
      tags_list: TagsList.t }
    let make ?resource_id  ?(tags_list= [])  () = { resource_id; tags_list }
    let parse xml =
      Some
        {
          resource_id =
            (Util.option_bind (Xml.member "ResourceId" xml) String.parse);
          tags_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "TagsList" xml) TagsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TagsList.member", (TagsList.to_query v.tags_list)));
           Util.option_map v.resource_id
             (fun f -> Query.Pair ("ResourceId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags_list", (TagsList.to_json v.tags_list));
           Util.option_map v.resource_id
             (fun f -> ("resource_id", (String.to_json f)))])
    let of_json j =
      {
        resource_id =
          (Util.option_map (Json.lookup j "resource_id") String.of_json);
        tags_list =
          (TagsList.of_json (Util.of_option_exn (Json.lookup j "tags_list")))
      }
  end
module EventSelector =
  struct
    type t =
      {
      read_write_type: ReadWriteType.t option ;
      include_management_events: Boolean.t option ;
      data_resources: DataResources.t ;
      exclude_management_event_sources: ExcludeManagementEventSources.t }
    let make ?read_write_type  ?include_management_events  ?(data_resources=
      [])  ?(exclude_management_event_sources= [])  () =
      {
        read_write_type;
        include_management_events;
        data_resources;
        exclude_management_event_sources
      }
    let parse xml =
      Some
        {
          read_write_type =
            (Util.option_bind (Xml.member "ReadWriteType" xml)
               ReadWriteType.parse);
          include_management_events =
            (Util.option_bind (Xml.member "IncludeManagementEvents" xml)
               Boolean.parse);
          data_resources =
            (Util.of_option []
               (Util.option_bind (Xml.member "DataResources" xml)
                  DataResources.parse));
          exclude_management_event_sources =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "ExcludeManagementEventSources" xml)
                  ExcludeManagementEventSources.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ExcludeManagementEventSources.member",
                   (ExcludeManagementEventSources.to_query
                      v.exclude_management_event_sources)));
           Some
             (Query.Pair
                ("DataResources.member",
                  (DataResources.to_query v.data_resources)));
           Util.option_map v.include_management_events
             (fun f ->
                Query.Pair ("IncludeManagementEvents", (Boolean.to_query f)));
           Util.option_map v.read_write_type
             (fun f ->
                Query.Pair ("ReadWriteType", (ReadWriteType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("exclude_management_event_sources",
                (ExcludeManagementEventSources.to_json
                   v.exclude_management_event_sources));
           Some ("data_resources", (DataResources.to_json v.data_resources));
           Util.option_map v.include_management_events
             (fun f -> ("include_management_events", (Boolean.to_json f)));
           Util.option_map v.read_write_type
             (fun f -> ("read_write_type", (ReadWriteType.to_json f)))])
    let of_json j =
      {
        read_write_type =
          (Util.option_map (Json.lookup j "read_write_type")
             ReadWriteType.of_json);
        include_management_events =
          (Util.option_map (Json.lookup j "include_management_events")
             Boolean.of_json);
        data_resources =
          (DataResources.of_json
             (Util.of_option_exn (Json.lookup j "data_resources")));
        exclude_management_event_sources =
          (ExcludeManagementEventSources.of_json
             (Util.of_option_exn
                (Json.lookup j "exclude_management_event_sources")))
      }
  end
module LookupAttribute =
  struct
    type t =
      {
      attribute_key: LookupAttributeKey.t ;
      attribute_value: String.t }
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
module TrailInfo =
  struct
    type t =
      {
      trail_a_r_n: String.t option ;
      name: String.t option ;
      home_region: String.t option }
    let make ?trail_a_r_n  ?name  ?home_region  () =
      { trail_a_r_n; name; home_region }
    let parse xml =
      Some
        {
          trail_a_r_n =
            (Util.option_bind (Xml.member "TrailARN" xml) String.parse);
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          home_region =
            (Util.option_bind (Xml.member "HomeRegion" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.home_region
              (fun f -> Query.Pair ("HomeRegion", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Util.option_map v.trail_a_r_n
             (fun f -> Query.Pair ("TrailARN", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.home_region
              (fun f -> ("home_region", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Util.option_map v.trail_a_r_n
             (fun f -> ("trail_a_r_n", (String.to_json f)))])
    let of_json j =
      {
        trail_a_r_n =
          (Util.option_map (Json.lookup j "trail_a_r_n") String.of_json);
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        home_region =
          (Util.option_map (Json.lookup j "home_region") String.of_json)
      }
  end
module Event =
  struct
    type t =
      {
      event_id: String.t option ;
      event_name: String.t option ;
      read_only: String.t option ;
      access_key_id: String.t option ;
      event_time: DateTime.t option ;
      event_source: String.t option ;
      username: String.t option ;
      resources: ResourceList.t ;
      cloud_trail_event: String.t option }
    let make ?event_id  ?event_name  ?read_only  ?access_key_id  ?event_time 
      ?event_source  ?username  ?(resources= [])  ?cloud_trail_event  () =
      {
        event_id;
        event_name;
        read_only;
        access_key_id;
        event_time;
        event_source;
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
          read_only =
            (Util.option_bind (Xml.member "ReadOnly" xml) String.parse);
          access_key_id =
            (Util.option_bind (Xml.member "AccessKeyId" xml) String.parse);
          event_time =
            (Util.option_bind (Xml.member "EventTime" xml) DateTime.parse);
          event_source =
            (Util.option_bind (Xml.member "EventSource" xml) String.parse);
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
              (fun f -> Query.Pair ("CloudTrailEvent", (String.to_query f)));
           Some
             (Query.Pair
                ("Resources.member", (ResourceList.to_query v.resources)));
           Util.option_map v.username
             (fun f -> Query.Pair ("Username", (String.to_query f)));
           Util.option_map v.event_source
             (fun f -> Query.Pair ("EventSource", (String.to_query f)));
           Util.option_map v.event_time
             (fun f -> Query.Pair ("EventTime", (DateTime.to_query f)));
           Util.option_map v.access_key_id
             (fun f -> Query.Pair ("AccessKeyId", (String.to_query f)));
           Util.option_map v.read_only
             (fun f -> Query.Pair ("ReadOnly", (String.to_query f)));
           Util.option_map v.event_name
             (fun f -> Query.Pair ("EventName", (String.to_query f)));
           Util.option_map v.event_id
             (fun f -> Query.Pair ("EventId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cloud_trail_event
              (fun f -> ("cloud_trail_event", (String.to_json f)));
           Some ("resources", (ResourceList.to_json v.resources));
           Util.option_map v.username
             (fun f -> ("username", (String.to_json f)));
           Util.option_map v.event_source
             (fun f -> ("event_source", (String.to_json f)));
           Util.option_map v.event_time
             (fun f -> ("event_time", (DateTime.to_json f)));
           Util.option_map v.access_key_id
             (fun f -> ("access_key_id", (String.to_json f)));
           Util.option_map v.read_only
             (fun f -> ("read_only", (String.to_json f)));
           Util.option_map v.event_name
             (fun f -> ("event_name", (String.to_json f)));
           Util.option_map v.event_id
             (fun f -> ("event_id", (String.to_json f)))])
    let of_json j =
      {
        event_id =
          (Util.option_map (Json.lookup j "event_id") String.of_json);
        event_name =
          (Util.option_map (Json.lookup j "event_name") String.of_json);
        read_only =
          (Util.option_map (Json.lookup j "read_only") String.of_json);
        access_key_id =
          (Util.option_map (Json.lookup j "access_key_id") String.of_json);
        event_time =
          (Util.option_map (Json.lookup j "event_time") DateTime.of_json);
        event_source =
          (Util.option_map (Json.lookup j "event_source") String.of_json);
        username =
          (Util.option_map (Json.lookup j "username") String.of_json);
        resources =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "resources")));
        cloud_trail_event =
          (Util.option_map (Json.lookup j "cloud_trail_event") String.of_json)
      }
  end
module PublicKey =
  struct
    type t =
      {
      value: Blob.t option ;
      validity_start_time: DateTime.t option ;
      validity_end_time: DateTime.t option ;
      fingerprint: String.t option }
    let make ?value  ?validity_start_time  ?validity_end_time  ?fingerprint 
      () = { value; validity_start_time; validity_end_time; fingerprint }
    let parse xml =
      Some
        {
          value = (Util.option_bind (Xml.member "Value" xml) Blob.parse);
          validity_start_time =
            (Util.option_bind (Xml.member "ValidityStartTime" xml)
               DateTime.parse);
          validity_end_time =
            (Util.option_bind (Xml.member "ValidityEndTime" xml)
               DateTime.parse);
          fingerprint =
            (Util.option_bind (Xml.member "Fingerprint" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.fingerprint
              (fun f -> Query.Pair ("Fingerprint", (String.to_query f)));
           Util.option_map v.validity_end_time
             (fun f -> Query.Pair ("ValidityEndTime", (DateTime.to_query f)));
           Util.option_map v.validity_start_time
             (fun f ->
                Query.Pair ("ValidityStartTime", (DateTime.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.fingerprint
              (fun f -> ("fingerprint", (String.to_json f)));
           Util.option_map v.validity_end_time
             (fun f -> ("validity_end_time", (DateTime.to_json f)));
           Util.option_map v.validity_start_time
             (fun f -> ("validity_start_time", (DateTime.to_json f)));
           Util.option_map v.value (fun f -> ("value", (Blob.to_json f)))])
    let of_json j =
      {
        value = (Util.option_map (Json.lookup j "value") Blob.of_json);
        validity_start_time =
          (Util.option_map (Json.lookup j "validity_start_time")
             DateTime.of_json);
        validity_end_time =
          (Util.option_map (Json.lookup j "validity_end_time")
             DateTime.of_json);
        fingerprint =
          (Util.option_map (Json.lookup j "fingerprint") String.of_json)
      }
  end
module Trail =
  struct
    type t =
      {
      name: String.t option ;
      s3_bucket_name: String.t option ;
      s3_key_prefix: String.t option ;
      sns_topic_name: String.t option ;
      sns_topic_a_r_n: String.t option ;
      include_global_service_events: Boolean.t option ;
      is_multi_region_trail: Boolean.t option ;
      home_region: String.t option ;
      trail_a_r_n: String.t option ;
      log_file_validation_enabled: Boolean.t option ;
      cloud_watch_logs_log_group_arn: String.t option ;
      cloud_watch_logs_role_arn: String.t option ;
      kms_key_id: String.t option ;
      has_custom_event_selectors: Boolean.t option ;
      has_insight_selectors: Boolean.t option ;
      is_organization_trail: Boolean.t option }
    let make ?name  ?s3_bucket_name  ?s3_key_prefix  ?sns_topic_name 
      ?sns_topic_a_r_n  ?include_global_service_events 
      ?is_multi_region_trail  ?home_region  ?trail_a_r_n 
      ?log_file_validation_enabled  ?cloud_watch_logs_log_group_arn 
      ?cloud_watch_logs_role_arn  ?kms_key_id  ?has_custom_event_selectors 
      ?has_insight_selectors  ?is_organization_trail  () =
      {
        name;
        s3_bucket_name;
        s3_key_prefix;
        sns_topic_name;
        sns_topic_a_r_n;
        include_global_service_events;
        is_multi_region_trail;
        home_region;
        trail_a_r_n;
        log_file_validation_enabled;
        cloud_watch_logs_log_group_arn;
        cloud_watch_logs_role_arn;
        kms_key_id;
        has_custom_event_selectors;
        has_insight_selectors;
        is_organization_trail
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
          sns_topic_a_r_n =
            (Util.option_bind (Xml.member "SnsTopicARN" xml) String.parse);
          include_global_service_events =
            (Util.option_bind (Xml.member "IncludeGlobalServiceEvents" xml)
               Boolean.parse);
          is_multi_region_trail =
            (Util.option_bind (Xml.member "IsMultiRegionTrail" xml)
               Boolean.parse);
          home_region =
            (Util.option_bind (Xml.member "HomeRegion" xml) String.parse);
          trail_a_r_n =
            (Util.option_bind (Xml.member "TrailARN" xml) String.parse);
          log_file_validation_enabled =
            (Util.option_bind (Xml.member "LogFileValidationEnabled" xml)
               Boolean.parse);
          cloud_watch_logs_log_group_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse);
          cloud_watch_logs_role_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsRoleArn" xml)
               String.parse);
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse);
          has_custom_event_selectors =
            (Util.option_bind (Xml.member "HasCustomEventSelectors" xml)
               Boolean.parse);
          has_insight_selectors =
            (Util.option_bind (Xml.member "HasInsightSelectors" xml)
               Boolean.parse);
          is_organization_trail =
            (Util.option_bind (Xml.member "IsOrganizationTrail" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.is_organization_trail
              (fun f ->
                 Query.Pair ("IsOrganizationTrail", (Boolean.to_query f)));
           Util.option_map v.has_insight_selectors
             (fun f ->
                Query.Pair ("HasInsightSelectors", (Boolean.to_query f)));
           Util.option_map v.has_custom_event_selectors
             (fun f ->
                Query.Pair ("HasCustomEventSelectors", (Boolean.to_query f)));
           Util.option_map v.kms_key_id
             (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_role_arn
             (fun f ->
                Query.Pair ("CloudWatchLogsRoleArn", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f ->
                Query.Pair ("CloudWatchLogsLogGroupArn", (String.to_query f)));
           Util.option_map v.log_file_validation_enabled
             (fun f ->
                Query.Pair ("LogFileValidationEnabled", (Boolean.to_query f)));
           Util.option_map v.trail_a_r_n
             (fun f -> Query.Pair ("TrailARN", (String.to_query f)));
           Util.option_map v.home_region
             (fun f -> Query.Pair ("HomeRegion", (String.to_query f)));
           Util.option_map v.is_multi_region_trail
             (fun f ->
                Query.Pair ("IsMultiRegionTrail", (Boolean.to_query f)));
           Util.option_map v.include_global_service_events
             (fun f ->
                Query.Pair
                  ("IncludeGlobalServiceEvents", (Boolean.to_query f)));
           Util.option_map v.sns_topic_a_r_n
             (fun f -> Query.Pair ("SnsTopicARN", (String.to_query f)));
           Util.option_map v.sns_topic_name
             (fun f -> Query.Pair ("SnsTopicName", (String.to_query f)));
           Util.option_map v.s3_key_prefix
             (fun f -> Query.Pair ("S3KeyPrefix", (String.to_query f)));
           Util.option_map v.s3_bucket_name
             (fun f -> Query.Pair ("S3BucketName", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.is_organization_trail
              (fun f -> ("is_organization_trail", (Boolean.to_json f)));
           Util.option_map v.has_insight_selectors
             (fun f -> ("has_insight_selectors", (Boolean.to_json f)));
           Util.option_map v.has_custom_event_selectors
             (fun f -> ("has_custom_event_selectors", (Boolean.to_json f)));
           Util.option_map v.kms_key_id
             (fun f -> ("kms_key_id", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_role_arn
             (fun f -> ("cloud_watch_logs_role_arn", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f -> ("cloud_watch_logs_log_group_arn", (String.to_json f)));
           Util.option_map v.log_file_validation_enabled
             (fun f -> ("log_file_validation_enabled", (Boolean.to_json f)));
           Util.option_map v.trail_a_r_n
             (fun f -> ("trail_a_r_n", (String.to_json f)));
           Util.option_map v.home_region
             (fun f -> ("home_region", (String.to_json f)));
           Util.option_map v.is_multi_region_trail
             (fun f -> ("is_multi_region_trail", (Boolean.to_json f)));
           Util.option_map v.include_global_service_events
             (fun f -> ("include_global_service_events", (Boolean.to_json f)));
           Util.option_map v.sns_topic_a_r_n
             (fun f -> ("sns_topic_a_r_n", (String.to_json f)));
           Util.option_map v.sns_topic_name
             (fun f -> ("sns_topic_name", (String.to_json f)));
           Util.option_map v.s3_key_prefix
             (fun f -> ("s3_key_prefix", (String.to_json f)));
           Util.option_map v.s3_bucket_name
             (fun f -> ("s3_bucket_name", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        s3_bucket_name =
          (Util.option_map (Json.lookup j "s3_bucket_name") String.of_json);
        s3_key_prefix =
          (Util.option_map (Json.lookup j "s3_key_prefix") String.of_json);
        sns_topic_name =
          (Util.option_map (Json.lookup j "sns_topic_name") String.of_json);
        sns_topic_a_r_n =
          (Util.option_map (Json.lookup j "sns_topic_a_r_n") String.of_json);
        include_global_service_events =
          (Util.option_map (Json.lookup j "include_global_service_events")
             Boolean.of_json);
        is_multi_region_trail =
          (Util.option_map (Json.lookup j "is_multi_region_trail")
             Boolean.of_json);
        home_region =
          (Util.option_map (Json.lookup j "home_region") String.of_json);
        trail_a_r_n =
          (Util.option_map (Json.lookup j "trail_a_r_n") String.of_json);
        log_file_validation_enabled =
          (Util.option_map (Json.lookup j "log_file_validation_enabled")
             Boolean.of_json);
        cloud_watch_logs_log_group_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_log_group_arn")
             String.of_json);
        cloud_watch_logs_role_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_role_arn")
             String.of_json);
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json);
        has_custom_event_selectors =
          (Util.option_map (Json.lookup j "has_custom_event_selectors")
             Boolean.of_json);
        has_insight_selectors =
          (Util.option_map (Json.lookup j "has_insight_selectors")
             Boolean.of_json);
        is_organization_trail =
          (Util.option_map (Json.lookup j "is_organization_trail")
             Boolean.of_json)
      }
  end
module InsightSelectors =
  struct
    type t = InsightSelector.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map InsightSelector.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list InsightSelector.to_query v
    let to_json v = `List (List.map InsightSelector.to_json v)
    let of_json j = Json.to_list InsightSelector.of_json j
  end
module ResourceIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ResourceTagList =
  struct
    type t = ResourceTag.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map ResourceTag.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ResourceTag.to_query v
    let to_json v = `List (List.map ResourceTag.to_json v)
    let of_json j = Json.to_list ResourceTag.of_json j
  end
module EventSelectors =
  struct
    type t = EventSelector.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map EventSelector.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list EventSelector.to_query v
    let to_json v = `List (List.map EventSelector.to_json v)
    let of_json j = Json.to_list EventSelector.of_json j
  end
module EventCategory =
  struct
    type t =
      | Insight 
    let str_to_t = [("insight", Insight)]
    let t_to_str = [(Insight, "insight")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
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
module Trails =
  struct
    type t = TrailInfo.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map TrailInfo.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list TrailInfo.to_query v
    let to_json v = `List (List.map TrailInfo.to_json v)
    let of_json j = Json.to_list TrailInfo.of_json j
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
module PublicKeyList =
  struct
    type t = PublicKey.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map PublicKey.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list PublicKey.to_query v
    let to_json v = `List (List.map PublicKey.to_json v)
    let of_json j = Json.to_list PublicKey.of_json j
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
module DeleteTrailResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GetInsightSelectorsResponse =
  struct
    type t =
      {
      trail_a_r_n: String.t option ;
      insight_selectors: InsightSelectors.t }
    let make ?trail_a_r_n  ?(insight_selectors= [])  () =
      { trail_a_r_n; insight_selectors }
    let parse xml =
      Some
        {
          trail_a_r_n =
            (Util.option_bind (Xml.member "TrailARN" xml) String.parse);
          insight_selectors =
            (Util.of_option []
               (Util.option_bind (Xml.member "InsightSelectors" xml)
                  InsightSelectors.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("InsightSelectors.member",
                   (InsightSelectors.to_query v.insight_selectors)));
           Util.option_map v.trail_a_r_n
             (fun f -> Query.Pair ("TrailARN", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("insight_selectors",
                (InsightSelectors.to_json v.insight_selectors));
           Util.option_map v.trail_a_r_n
             (fun f -> ("trail_a_r_n", (String.to_json f)))])
    let of_json j =
      {
        trail_a_r_n =
          (Util.option_map (Json.lookup j "trail_a_r_n") String.of_json);
        insight_selectors =
          (InsightSelectors.of_json
             (Util.of_option_exn (Json.lookup j "insight_selectors")))
      }
  end
module ListTagsRequest =
  struct
    type t =
      {
      resource_id_list: ResourceIdList.t ;
      next_token: String.t option }
    let make ~resource_id_list  ?next_token  () =
      { resource_id_list; next_token }
    let parse xml =
      Some
        {
          resource_id_list =
            (Xml.required "ResourceIdList"
               (Util.option_bind (Xml.member "ResourceIdList" xml)
                  ResourceIdList.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("ResourceIdList.member",
                  (ResourceIdList.to_query v.resource_id_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("resource_id_list",
               (ResourceIdList.to_json v.resource_id_list))])
    let of_json j =
      {
        resource_id_list =
          (ResourceIdList.of_json
             (Util.of_option_exn (Json.lookup j "resource_id_list")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module StopLoggingRequest =
  struct
    type t = {
      name: String.t }
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
module ListTagsResponse =
  struct
    type t =
      {
      resource_tag_list: ResourceTagList.t ;
      next_token: String.t option }
    let make ?(resource_tag_list= [])  ?next_token  () =
      { resource_tag_list; next_token }
    let parse xml =
      Some
        {
          resource_tag_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "ResourceTagList" xml)
                  ResourceTagList.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("ResourceTagList.member",
                  (ResourceTagList.to_query v.resource_tag_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("resource_tag_list",
               (ResourceTagList.to_json v.resource_tag_list))])
    let of_json j =
      {
        resource_tag_list =
          (ResourceTagList.of_json
             (Util.of_option_exn (Json.lookup j "resource_tag_list")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module TrailAlreadyExistsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module StartLoggingRequest =
  struct
    type t = {
      name: String.t }
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
module ResourceTypeNotSupportedException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidS3BucketNameException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RemoveTagsRequest =
  struct
    type t = {
      resource_id: String.t ;
      tags_list: TagsList.t }
    let make ~resource_id  ?(tags_list= [])  () = { resource_id; tags_list }
    let parse xml =
      Some
        {
          resource_id =
            (Xml.required "ResourceId"
               (Util.option_bind (Xml.member "ResourceId" xml) String.parse));
          tags_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "TagsList" xml) TagsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TagsList.member", (TagsList.to_query v.tags_list)));
           Some (Query.Pair ("ResourceId", (String.to_query v.resource_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags_list", (TagsList.to_json v.tags_list));
           Some ("resource_id", (String.to_json v.resource_id))])
    let of_json j =
      {
        resource_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "resource_id")));
        tags_list =
          (TagsList.of_json (Util.of_option_exn (Json.lookup j "tags_list")))
      }
  end
module InvalidCloudWatchLogsRoleArnException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module PutEventSelectorsResponse =
  struct
    type t =
      {
      trail_a_r_n: String.t option ;
      event_selectors: EventSelectors.t }
    let make ?trail_a_r_n  ?(event_selectors= [])  () =
      { trail_a_r_n; event_selectors }
    let parse xml =
      Some
        {
          trail_a_r_n =
            (Util.option_bind (Xml.member "TrailARN" xml) String.parse);
          event_selectors =
            (Util.of_option []
               (Util.option_bind (Xml.member "EventSelectors" xml)
                  EventSelectors.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EventSelectors.member",
                   (EventSelectors.to_query v.event_selectors)));
           Util.option_map v.trail_a_r_n
             (fun f -> Query.Pair ("TrailARN", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("event_selectors", (EventSelectors.to_json v.event_selectors));
           Util.option_map v.trail_a_r_n
             (fun f -> ("trail_a_r_n", (String.to_json f)))])
    let of_json j =
      {
        trail_a_r_n =
          (Util.option_map (Json.lookup j "trail_a_r_n") String.of_json);
        event_selectors =
          (EventSelectors.of_json
             (Util.of_option_exn (Json.lookup j "event_selectors")))
      }
  end
module InsufficientDependencyServiceAccessPermissionException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module LookupEventsRequest =
  struct
    type t =
      {
      lookup_attributes: LookupAttributesList.t ;
      start_time: DateTime.t option ;
      end_time: DateTime.t option ;
      event_category: EventCategory.t option ;
      max_results: Integer.t option ;
      next_token: String.t option }
    let make ?(lookup_attributes= [])  ?start_time  ?end_time 
      ?event_category  ?max_results  ?next_token  () =
      {
        lookup_attributes;
        start_time;
        end_time;
        event_category;
        max_results;
        next_token
      }
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
          event_category =
            (Util.option_bind (Xml.member "EventCategory" xml)
               EventCategory.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.event_category
             (fun f ->
                Query.Pair ("EventCategory", (EventCategory.to_query f)));
           Util.option_map v.end_time
             (fun f -> Query.Pair ("EndTime", (DateTime.to_query f)));
           Util.option_map v.start_time
             (fun f -> Query.Pair ("StartTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("LookupAttributes.member",
                  (LookupAttributesList.to_query v.lookup_attributes)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.event_category
             (fun f -> ("event_category", (EventCategory.to_json f)));
           Util.option_map v.end_time
             (fun f -> ("end_time", (DateTime.to_json f)));
           Util.option_map v.start_time
             (fun f -> ("start_time", (DateTime.to_json f)));
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
        event_category =
          (Util.option_map (Json.lookup j "event_category")
             EventCategory.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module InvalidTokenException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module StopLoggingResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GetTrailStatusResponse =
  struct
    type t =
      {
      is_logging: Boolean.t option ;
      latest_delivery_error: String.t option ;
      latest_notification_error: String.t option ;
      latest_delivery_time: DateTime.t option ;
      latest_notification_time: DateTime.t option ;
      start_logging_time: DateTime.t option ;
      stop_logging_time: DateTime.t option ;
      latest_cloud_watch_logs_delivery_error: String.t option ;
      latest_cloud_watch_logs_delivery_time: DateTime.t option ;
      latest_digest_delivery_time: DateTime.t option ;
      latest_digest_delivery_error: String.t option ;
      latest_delivery_attempt_time: String.t option ;
      latest_notification_attempt_time: String.t option ;
      latest_notification_attempt_succeeded: String.t option ;
      latest_delivery_attempt_succeeded: String.t option ;
      time_logging_started: String.t option ;
      time_logging_stopped: String.t option }
    let make ?is_logging  ?latest_delivery_error  ?latest_notification_error 
      ?latest_delivery_time  ?latest_notification_time  ?start_logging_time 
      ?stop_logging_time  ?latest_cloud_watch_logs_delivery_error 
      ?latest_cloud_watch_logs_delivery_time  ?latest_digest_delivery_time 
      ?latest_digest_delivery_error  ?latest_delivery_attempt_time 
      ?latest_notification_attempt_time 
      ?latest_notification_attempt_succeeded 
      ?latest_delivery_attempt_succeeded  ?time_logging_started 
      ?time_logging_stopped  () =
      {
        is_logging;
        latest_delivery_error;
        latest_notification_error;
        latest_delivery_time;
        latest_notification_time;
        start_logging_time;
        stop_logging_time;
        latest_cloud_watch_logs_delivery_error;
        latest_cloud_watch_logs_delivery_time;
        latest_digest_delivery_time;
        latest_digest_delivery_error;
        latest_delivery_attempt_time;
        latest_notification_attempt_time;
        latest_notification_attempt_succeeded;
        latest_delivery_attempt_succeeded;
        time_logging_started;
        time_logging_stopped
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
               DateTime.parse);
          latest_digest_delivery_time =
            (Util.option_bind (Xml.member "LatestDigestDeliveryTime" xml)
               DateTime.parse);
          latest_digest_delivery_error =
            (Util.option_bind (Xml.member "LatestDigestDeliveryError" xml)
               String.parse);
          latest_delivery_attempt_time =
            (Util.option_bind (Xml.member "LatestDeliveryAttemptTime" xml)
               String.parse);
          latest_notification_attempt_time =
            (Util.option_bind
               (Xml.member "LatestNotificationAttemptTime" xml) String.parse);
          latest_notification_attempt_succeeded =
            (Util.option_bind
               (Xml.member "LatestNotificationAttemptSucceeded" xml)
               String.parse);
          latest_delivery_attempt_succeeded =
            (Util.option_bind
               (Xml.member "LatestDeliveryAttemptSucceeded" xml) String.parse);
          time_logging_started =
            (Util.option_bind (Xml.member "TimeLoggingStarted" xml)
               String.parse);
          time_logging_stopped =
            (Util.option_bind (Xml.member "TimeLoggingStopped" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.time_logging_stopped
              (fun f ->
                 Query.Pair ("TimeLoggingStopped", (String.to_query f)));
           Util.option_map v.time_logging_started
             (fun f -> Query.Pair ("TimeLoggingStarted", (String.to_query f)));
           Util.option_map v.latest_delivery_attempt_succeeded
             (fun f ->
                Query.Pair
                  ("LatestDeliveryAttemptSucceeded", (String.to_query f)));
           Util.option_map v.latest_notification_attempt_succeeded
             (fun f ->
                Query.Pair
                  ("LatestNotificationAttemptSucceeded", (String.to_query f)));
           Util.option_map v.latest_notification_attempt_time
             (fun f ->
                Query.Pair
                  ("LatestNotificationAttemptTime", (String.to_query f)));
           Util.option_map v.latest_delivery_attempt_time
             (fun f ->
                Query.Pair ("LatestDeliveryAttemptTime", (String.to_query f)));
           Util.option_map v.latest_digest_delivery_error
             (fun f ->
                Query.Pair ("LatestDigestDeliveryError", (String.to_query f)));
           Util.option_map v.latest_digest_delivery_time
             (fun f ->
                Query.Pair
                  ("LatestDigestDeliveryTime", (DateTime.to_query f)));
           Util.option_map v.latest_cloud_watch_logs_delivery_time
             (fun f ->
                Query.Pair
                  ("LatestCloudWatchLogsDeliveryTime", (DateTime.to_query f)));
           Util.option_map v.latest_cloud_watch_logs_delivery_error
             (fun f ->
                Query.Pair
                  ("LatestCloudWatchLogsDeliveryError", (String.to_query f)));
           Util.option_map v.stop_logging_time
             (fun f -> Query.Pair ("StopLoggingTime", (DateTime.to_query f)));
           Util.option_map v.start_logging_time
             (fun f -> Query.Pair ("StartLoggingTime", (DateTime.to_query f)));
           Util.option_map v.latest_notification_time
             (fun f ->
                Query.Pair ("LatestNotificationTime", (DateTime.to_query f)));
           Util.option_map v.latest_delivery_time
             (fun f ->
                Query.Pair ("LatestDeliveryTime", (DateTime.to_query f)));
           Util.option_map v.latest_notification_error
             (fun f ->
                Query.Pair ("LatestNotificationError", (String.to_query f)));
           Util.option_map v.latest_delivery_error
             (fun f ->
                Query.Pair ("LatestDeliveryError", (String.to_query f)));
           Util.option_map v.is_logging
             (fun f -> Query.Pair ("IsLogging", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.time_logging_stopped
              (fun f -> ("time_logging_stopped", (String.to_json f)));
           Util.option_map v.time_logging_started
             (fun f -> ("time_logging_started", (String.to_json f)));
           Util.option_map v.latest_delivery_attempt_succeeded
             (fun f ->
                ("latest_delivery_attempt_succeeded", (String.to_json f)));
           Util.option_map v.latest_notification_attempt_succeeded
             (fun f ->
                ("latest_notification_attempt_succeeded", (String.to_json f)));
           Util.option_map v.latest_notification_attempt_time
             (fun f ->
                ("latest_notification_attempt_time", (String.to_json f)));
           Util.option_map v.latest_delivery_attempt_time
             (fun f -> ("latest_delivery_attempt_time", (String.to_json f)));
           Util.option_map v.latest_digest_delivery_error
             (fun f -> ("latest_digest_delivery_error", (String.to_json f)));
           Util.option_map v.latest_digest_delivery_time
             (fun f -> ("latest_digest_delivery_time", (DateTime.to_json f)));
           Util.option_map v.latest_cloud_watch_logs_delivery_time
             (fun f ->
                ("latest_cloud_watch_logs_delivery_time",
                  (DateTime.to_json f)));
           Util.option_map v.latest_cloud_watch_logs_delivery_error
             (fun f ->
                ("latest_cloud_watch_logs_delivery_error",
                  (String.to_json f)));
           Util.option_map v.stop_logging_time
             (fun f -> ("stop_logging_time", (DateTime.to_json f)));
           Util.option_map v.start_logging_time
             (fun f -> ("start_logging_time", (DateTime.to_json f)));
           Util.option_map v.latest_notification_time
             (fun f -> ("latest_notification_time", (DateTime.to_json f)));
           Util.option_map v.latest_delivery_time
             (fun f -> ("latest_delivery_time", (DateTime.to_json f)));
           Util.option_map v.latest_notification_error
             (fun f -> ("latest_notification_error", (String.to_json f)));
           Util.option_map v.latest_delivery_error
             (fun f -> ("latest_delivery_error", (String.to_json f)));
           Util.option_map v.is_logging
             (fun f -> ("is_logging", (Boolean.to_json f)))])
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
             DateTime.of_json);
        latest_digest_delivery_time =
          (Util.option_map (Json.lookup j "latest_digest_delivery_time")
             DateTime.of_json);
        latest_digest_delivery_error =
          (Util.option_map (Json.lookup j "latest_digest_delivery_error")
             String.of_json);
        latest_delivery_attempt_time =
          (Util.option_map (Json.lookup j "latest_delivery_attempt_time")
             String.of_json);
        latest_notification_attempt_time =
          (Util.option_map (Json.lookup j "latest_notification_attempt_time")
             String.of_json);
        latest_notification_attempt_succeeded =
          (Util.option_map
             (Json.lookup j "latest_notification_attempt_succeeded")
             String.of_json);
        latest_delivery_attempt_succeeded =
          (Util.option_map
             (Json.lookup j "latest_delivery_attempt_succeeded")
             String.of_json);
        time_logging_started =
          (Util.option_map (Json.lookup j "time_logging_started")
             String.of_json);
        time_logging_stopped =
          (Util.option_map (Json.lookup j "time_logging_stopped")
             String.of_json)
      }
  end
module InvalidKmsKeyIdException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module KmsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidInsightSelectorsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module TrailNotProvidedException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListTrailsResponse =
  struct
    type t = {
      trails: Trails.t ;
      next_token: String.t option }
    let make ?(trails= [])  ?next_token  () = { trails; next_token }
    let parse xml =
      Some
        {
          trails =
            (Util.of_option []
               (Util.option_bind (Xml.member "Trails" xml) Trails.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("Trails.member", (Trails.to_query v.trails)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("trails", (Trails.to_json v.trails))])
    let of_json j =
      {
        trails =
          (Trails.of_json (Util.of_option_exn (Json.lookup j "trails")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DeleteTrailRequest =
  struct
    type t = {
      name: String.t }
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
module GetTrailResponse =
  struct
    type t = {
      trail: Trail.t option }
    let make ?trail  () = { trail }
    let parse xml =
      Some
        { trail = (Util.option_bind (Xml.member "Trail" xml) Trail.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.trail
              (fun f -> Query.Pair ("Trail", (Trail.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.trail (fun f -> ("trail", (Trail.to_json f)))])
    let of_json j =
      { trail = (Util.option_map (Json.lookup j "trail") Trail.of_json) }
  end
module KmsKeyDisabledException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module TrailNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidTagParameterException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module UpdateTrailResponse =
  struct
    type t =
      {
      name: String.t option ;
      s3_bucket_name: String.t option ;
      s3_key_prefix: String.t option ;
      sns_topic_name: String.t option ;
      sns_topic_a_r_n: String.t option ;
      include_global_service_events: Boolean.t option ;
      is_multi_region_trail: Boolean.t option ;
      trail_a_r_n: String.t option ;
      log_file_validation_enabled: Boolean.t option ;
      cloud_watch_logs_log_group_arn: String.t option ;
      cloud_watch_logs_role_arn: String.t option ;
      kms_key_id: String.t option ;
      is_organization_trail: Boolean.t option }
    let make ?name  ?s3_bucket_name  ?s3_key_prefix  ?sns_topic_name 
      ?sns_topic_a_r_n  ?include_global_service_events 
      ?is_multi_region_trail  ?trail_a_r_n  ?log_file_validation_enabled 
      ?cloud_watch_logs_log_group_arn  ?cloud_watch_logs_role_arn 
      ?kms_key_id  ?is_organization_trail  () =
      {
        name;
        s3_bucket_name;
        s3_key_prefix;
        sns_topic_name;
        sns_topic_a_r_n;
        include_global_service_events;
        is_multi_region_trail;
        trail_a_r_n;
        log_file_validation_enabled;
        cloud_watch_logs_log_group_arn;
        cloud_watch_logs_role_arn;
        kms_key_id;
        is_organization_trail
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
          sns_topic_a_r_n =
            (Util.option_bind (Xml.member "SnsTopicARN" xml) String.parse);
          include_global_service_events =
            (Util.option_bind (Xml.member "IncludeGlobalServiceEvents" xml)
               Boolean.parse);
          is_multi_region_trail =
            (Util.option_bind (Xml.member "IsMultiRegionTrail" xml)
               Boolean.parse);
          trail_a_r_n =
            (Util.option_bind (Xml.member "TrailARN" xml) String.parse);
          log_file_validation_enabled =
            (Util.option_bind (Xml.member "LogFileValidationEnabled" xml)
               Boolean.parse);
          cloud_watch_logs_log_group_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse);
          cloud_watch_logs_role_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsRoleArn" xml)
               String.parse);
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse);
          is_organization_trail =
            (Util.option_bind (Xml.member "IsOrganizationTrail" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.is_organization_trail
              (fun f ->
                 Query.Pair ("IsOrganizationTrail", (Boolean.to_query f)));
           Util.option_map v.kms_key_id
             (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_role_arn
             (fun f ->
                Query.Pair ("CloudWatchLogsRoleArn", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f ->
                Query.Pair ("CloudWatchLogsLogGroupArn", (String.to_query f)));
           Util.option_map v.log_file_validation_enabled
             (fun f ->
                Query.Pair ("LogFileValidationEnabled", (Boolean.to_query f)));
           Util.option_map v.trail_a_r_n
             (fun f -> Query.Pair ("TrailARN", (String.to_query f)));
           Util.option_map v.is_multi_region_trail
             (fun f ->
                Query.Pair ("IsMultiRegionTrail", (Boolean.to_query f)));
           Util.option_map v.include_global_service_events
             (fun f ->
                Query.Pair
                  ("IncludeGlobalServiceEvents", (Boolean.to_query f)));
           Util.option_map v.sns_topic_a_r_n
             (fun f -> Query.Pair ("SnsTopicARN", (String.to_query f)));
           Util.option_map v.sns_topic_name
             (fun f -> Query.Pair ("SnsTopicName", (String.to_query f)));
           Util.option_map v.s3_key_prefix
             (fun f -> Query.Pair ("S3KeyPrefix", (String.to_query f)));
           Util.option_map v.s3_bucket_name
             (fun f -> Query.Pair ("S3BucketName", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.is_organization_trail
              (fun f -> ("is_organization_trail", (Boolean.to_json f)));
           Util.option_map v.kms_key_id
             (fun f -> ("kms_key_id", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_role_arn
             (fun f -> ("cloud_watch_logs_role_arn", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f -> ("cloud_watch_logs_log_group_arn", (String.to_json f)));
           Util.option_map v.log_file_validation_enabled
             (fun f -> ("log_file_validation_enabled", (Boolean.to_json f)));
           Util.option_map v.trail_a_r_n
             (fun f -> ("trail_a_r_n", (String.to_json f)));
           Util.option_map v.is_multi_region_trail
             (fun f -> ("is_multi_region_trail", (Boolean.to_json f)));
           Util.option_map v.include_global_service_events
             (fun f -> ("include_global_service_events", (Boolean.to_json f)));
           Util.option_map v.sns_topic_a_r_n
             (fun f -> ("sns_topic_a_r_n", (String.to_json f)));
           Util.option_map v.sns_topic_name
             (fun f -> ("sns_topic_name", (String.to_json f)));
           Util.option_map v.s3_key_prefix
             (fun f -> ("s3_key_prefix", (String.to_json f)));
           Util.option_map v.s3_bucket_name
             (fun f -> ("s3_bucket_name", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        s3_bucket_name =
          (Util.option_map (Json.lookup j "s3_bucket_name") String.of_json);
        s3_key_prefix =
          (Util.option_map (Json.lookup j "s3_key_prefix") String.of_json);
        sns_topic_name =
          (Util.option_map (Json.lookup j "sns_topic_name") String.of_json);
        sns_topic_a_r_n =
          (Util.option_map (Json.lookup j "sns_topic_a_r_n") String.of_json);
        include_global_service_events =
          (Util.option_map (Json.lookup j "include_global_service_events")
             Boolean.of_json);
        is_multi_region_trail =
          (Util.option_map (Json.lookup j "is_multi_region_trail")
             Boolean.of_json);
        trail_a_r_n =
          (Util.option_map (Json.lookup j "trail_a_r_n") String.of_json);
        log_file_validation_enabled =
          (Util.option_map (Json.lookup j "log_file_validation_enabled")
             Boolean.of_json);
        cloud_watch_logs_log_group_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_log_group_arn")
             String.of_json);
        cloud_watch_logs_role_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_role_arn")
             String.of_json);
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json);
        is_organization_trail =
          (Util.option_map (Json.lookup j "is_organization_trail")
             Boolean.of_json)
      }
  end
module CreateTrailRequest =
  struct
    type t =
      {
      name: String.t ;
      s3_bucket_name: String.t ;
      s3_key_prefix: String.t option ;
      sns_topic_name: String.t option ;
      include_global_service_events: Boolean.t option ;
      is_multi_region_trail: Boolean.t option ;
      enable_log_file_validation: Boolean.t option ;
      cloud_watch_logs_log_group_arn: String.t option ;
      cloud_watch_logs_role_arn: String.t option ;
      kms_key_id: String.t option ;
      is_organization_trail: Boolean.t option ;
      tags_list: TagsList.t }
    let make ~name  ~s3_bucket_name  ?s3_key_prefix  ?sns_topic_name 
      ?include_global_service_events  ?is_multi_region_trail 
      ?enable_log_file_validation  ?cloud_watch_logs_log_group_arn 
      ?cloud_watch_logs_role_arn  ?kms_key_id  ?is_organization_trail 
      ?(tags_list= [])  () =
      {
        name;
        s3_bucket_name;
        s3_key_prefix;
        sns_topic_name;
        include_global_service_events;
        is_multi_region_trail;
        enable_log_file_validation;
        cloud_watch_logs_log_group_arn;
        cloud_watch_logs_role_arn;
        kms_key_id;
        is_organization_trail;
        tags_list
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
          is_multi_region_trail =
            (Util.option_bind (Xml.member "IsMultiRegionTrail" xml)
               Boolean.parse);
          enable_log_file_validation =
            (Util.option_bind (Xml.member "EnableLogFileValidation" xml)
               Boolean.parse);
          cloud_watch_logs_log_group_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse);
          cloud_watch_logs_role_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsRoleArn" xml)
               String.parse);
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse);
          is_organization_trail =
            (Util.option_bind (Xml.member "IsOrganizationTrail" xml)
               Boolean.parse);
          tags_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "TagsList" xml) TagsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TagsList.member", (TagsList.to_query v.tags_list)));
           Util.option_map v.is_organization_trail
             (fun f ->
                Query.Pair ("IsOrganizationTrail", (Boolean.to_query f)));
           Util.option_map v.kms_key_id
             (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_role_arn
             (fun f ->
                Query.Pair ("CloudWatchLogsRoleArn", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f ->
                Query.Pair ("CloudWatchLogsLogGroupArn", (String.to_query f)));
           Util.option_map v.enable_log_file_validation
             (fun f ->
                Query.Pair ("EnableLogFileValidation", (Boolean.to_query f)));
           Util.option_map v.is_multi_region_trail
             (fun f ->
                Query.Pair ("IsMultiRegionTrail", (Boolean.to_query f)));
           Util.option_map v.include_global_service_events
             (fun f ->
                Query.Pair
                  ("IncludeGlobalServiceEvents", (Boolean.to_query f)));
           Util.option_map v.sns_topic_name
             (fun f -> Query.Pair ("SnsTopicName", (String.to_query f)));
           Util.option_map v.s3_key_prefix
             (fun f -> Query.Pair ("S3KeyPrefix", (String.to_query f)));
           Some
             (Query.Pair ("S3BucketName", (String.to_query v.s3_bucket_name)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags_list", (TagsList.to_json v.tags_list));
           Util.option_map v.is_organization_trail
             (fun f -> ("is_organization_trail", (Boolean.to_json f)));
           Util.option_map v.kms_key_id
             (fun f -> ("kms_key_id", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_role_arn
             (fun f -> ("cloud_watch_logs_role_arn", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f -> ("cloud_watch_logs_log_group_arn", (String.to_json f)));
           Util.option_map v.enable_log_file_validation
             (fun f -> ("enable_log_file_validation", (Boolean.to_json f)));
           Util.option_map v.is_multi_region_trail
             (fun f -> ("is_multi_region_trail", (Boolean.to_json f)));
           Util.option_map v.include_global_service_events
             (fun f -> ("include_global_service_events", (Boolean.to_json f)));
           Util.option_map v.sns_topic_name
             (fun f -> ("sns_topic_name", (String.to_json f)));
           Util.option_map v.s3_key_prefix
             (fun f -> ("s3_key_prefix", (String.to_json f)));
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
        is_multi_region_trail =
          (Util.option_map (Json.lookup j "is_multi_region_trail")
             Boolean.of_json);
        enable_log_file_validation =
          (Util.option_map (Json.lookup j "enable_log_file_validation")
             Boolean.of_json);
        cloud_watch_logs_log_group_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_log_group_arn")
             String.of_json);
        cloud_watch_logs_role_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_role_arn")
             String.of_json);
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json);
        is_organization_trail =
          (Util.option_map (Json.lookup j "is_organization_trail")
             Boolean.of_json);
        tags_list =
          (TagsList.of_json (Util.of_option_exn (Json.lookup j "tags_list")))
      }
  end
module AddTagsResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module StartLoggingResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListTrailsRequest =
  struct
    type t = {
      next_token: String.t option }
    let make ?next_token  () = { next_token }
    let parse xml =
      Some
        {
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)))])
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module OrganizationNotInAllFeaturesModeException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module PutInsightSelectorsRequest =
  struct
    type t = {
      trail_name: String.t ;
      insight_selectors: InsightSelectors.t }
    let make ~trail_name  ~insight_selectors  () =
      { trail_name; insight_selectors }
    let parse xml =
      Some
        {
          trail_name =
            (Xml.required "TrailName"
               (Util.option_bind (Xml.member "TrailName" xml) String.parse));
          insight_selectors =
            (Xml.required "InsightSelectors"
               (Util.option_bind (Xml.member "InsightSelectors" xml)
                  InsightSelectors.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("InsightSelectors.member",
                   (InsightSelectors.to_query v.insight_selectors)));
           Some (Query.Pair ("TrailName", (String.to_query v.trail_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("insight_selectors",
                (InsightSelectors.to_json v.insight_selectors));
           Some ("trail_name", (String.to_json v.trail_name))])
    let of_json j =
      {
        trail_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "trail_name")));
        insight_selectors =
          (InsightSelectors.of_json
             (Util.of_option_exn (Json.lookup j "insight_selectors")))
      }
  end
module GetEventSelectorsRequest =
  struct
    type t = {
      trail_name: String.t }
    let make ~trail_name  () = { trail_name }
    let parse xml =
      Some
        {
          trail_name =
            (Xml.required "TrailName"
               (Util.option_bind (Xml.member "TrailName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("TrailName", (String.to_query v.trail_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("trail_name", (String.to_json v.trail_name))])
    let of_json j =
      {
        trail_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "trail_name")))
      }
  end
module PutEventSelectorsRequest =
  struct
    type t = {
      trail_name: String.t ;
      event_selectors: EventSelectors.t }
    let make ~trail_name  ~event_selectors  () =
      { trail_name; event_selectors }
    let parse xml =
      Some
        {
          trail_name =
            (Xml.required "TrailName"
               (Util.option_bind (Xml.member "TrailName" xml) String.parse));
          event_selectors =
            (Xml.required "EventSelectors"
               (Util.option_bind (Xml.member "EventSelectors" xml)
                  EventSelectors.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EventSelectors.member",
                   (EventSelectors.to_query v.event_selectors)));
           Some (Query.Pair ("TrailName", (String.to_query v.trail_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("event_selectors", (EventSelectors.to_json v.event_selectors));
           Some ("trail_name", (String.to_json v.trail_name))])
    let of_json j =
      {
        trail_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "trail_name")));
        event_selectors =
          (EventSelectors.of_json
             (Util.of_option_exn (Json.lookup j "event_selectors")))
      }
  end
module AddTagsRequest =
  struct
    type t = {
      resource_id: String.t ;
      tags_list: TagsList.t }
    let make ~resource_id  ?(tags_list= [])  () = { resource_id; tags_list }
    let parse xml =
      Some
        {
          resource_id =
            (Xml.required "ResourceId"
               (Util.option_bind (Xml.member "ResourceId" xml) String.parse));
          tags_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "TagsList" xml) TagsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TagsList.member", (TagsList.to_query v.tags_list)));
           Some (Query.Pair ("ResourceId", (String.to_query v.resource_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags_list", (TagsList.to_json v.tags_list));
           Some ("resource_id", (String.to_json v.resource_id))])
    let of_json j =
      {
        resource_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "resource_id")));
        tags_list =
          (TagsList.of_json (Util.of_option_exn (Json.lookup j "tags_list")))
      }
  end
module GetTrailStatusRequest =
  struct
    type t = {
      name: String.t }
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
      name: String.t ;
      s3_bucket_name: String.t option ;
      s3_key_prefix: String.t option ;
      sns_topic_name: String.t option ;
      include_global_service_events: Boolean.t option ;
      is_multi_region_trail: Boolean.t option ;
      enable_log_file_validation: Boolean.t option ;
      cloud_watch_logs_log_group_arn: String.t option ;
      cloud_watch_logs_role_arn: String.t option ;
      kms_key_id: String.t option ;
      is_organization_trail: Boolean.t option }
    let make ~name  ?s3_bucket_name  ?s3_key_prefix  ?sns_topic_name 
      ?include_global_service_events  ?is_multi_region_trail 
      ?enable_log_file_validation  ?cloud_watch_logs_log_group_arn 
      ?cloud_watch_logs_role_arn  ?kms_key_id  ?is_organization_trail  () =
      {
        name;
        s3_bucket_name;
        s3_key_prefix;
        sns_topic_name;
        include_global_service_events;
        is_multi_region_trail;
        enable_log_file_validation;
        cloud_watch_logs_log_group_arn;
        cloud_watch_logs_role_arn;
        kms_key_id;
        is_organization_trail
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
          is_multi_region_trail =
            (Util.option_bind (Xml.member "IsMultiRegionTrail" xml)
               Boolean.parse);
          enable_log_file_validation =
            (Util.option_bind (Xml.member "EnableLogFileValidation" xml)
               Boolean.parse);
          cloud_watch_logs_log_group_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse);
          cloud_watch_logs_role_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsRoleArn" xml)
               String.parse);
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse);
          is_organization_trail =
            (Util.option_bind (Xml.member "IsOrganizationTrail" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.is_organization_trail
              (fun f ->
                 Query.Pair ("IsOrganizationTrail", (Boolean.to_query f)));
           Util.option_map v.kms_key_id
             (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_role_arn
             (fun f ->
                Query.Pair ("CloudWatchLogsRoleArn", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f ->
                Query.Pair ("CloudWatchLogsLogGroupArn", (String.to_query f)));
           Util.option_map v.enable_log_file_validation
             (fun f ->
                Query.Pair ("EnableLogFileValidation", (Boolean.to_query f)));
           Util.option_map v.is_multi_region_trail
             (fun f ->
                Query.Pair ("IsMultiRegionTrail", (Boolean.to_query f)));
           Util.option_map v.include_global_service_events
             (fun f ->
                Query.Pair
                  ("IncludeGlobalServiceEvents", (Boolean.to_query f)));
           Util.option_map v.sns_topic_name
             (fun f -> Query.Pair ("SnsTopicName", (String.to_query f)));
           Util.option_map v.s3_key_prefix
             (fun f -> Query.Pair ("S3KeyPrefix", (String.to_query f)));
           Util.option_map v.s3_bucket_name
             (fun f -> Query.Pair ("S3BucketName", (String.to_query f)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.is_organization_trail
              (fun f -> ("is_organization_trail", (Boolean.to_json f)));
           Util.option_map v.kms_key_id
             (fun f -> ("kms_key_id", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_role_arn
             (fun f -> ("cloud_watch_logs_role_arn", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f -> ("cloud_watch_logs_log_group_arn", (String.to_json f)));
           Util.option_map v.enable_log_file_validation
             (fun f -> ("enable_log_file_validation", (Boolean.to_json f)));
           Util.option_map v.is_multi_region_trail
             (fun f -> ("is_multi_region_trail", (Boolean.to_json f)));
           Util.option_map v.include_global_service_events
             (fun f -> ("include_global_service_events", (Boolean.to_json f)));
           Util.option_map v.sns_topic_name
             (fun f -> ("sns_topic_name", (String.to_json f)));
           Util.option_map v.s3_key_prefix
             (fun f -> ("s3_key_prefix", (String.to_json f)));
           Util.option_map v.s3_bucket_name
             (fun f -> ("s3_bucket_name", (String.to_json f)));
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
        is_multi_region_trail =
          (Util.option_map (Json.lookup j "is_multi_region_trail")
             Boolean.of_json);
        enable_log_file_validation =
          (Util.option_map (Json.lookup j "enable_log_file_validation")
             Boolean.of_json);
        cloud_watch_logs_log_group_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_log_group_arn")
             String.of_json);
        cloud_watch_logs_role_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_role_arn")
             String.of_json);
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json);
        is_organization_trail =
          (Util.option_map (Json.lookup j "is_organization_trail")
             Boolean.of_json)
      }
  end
module InvalidLookupAttributesException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidTrailNameException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module S3BucketDoesNotExistException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeTrailsRequest =
  struct
    type t =
      {
      trail_name_list: TrailNameList.t ;
      include_shadow_trails: Boolean.t option }
    let make ?(trail_name_list= [])  ?include_shadow_trails  () =
      { trail_name_list; include_shadow_trails }
    let parse xml =
      Some
        {
          trail_name_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "trailNameList" xml)
                  TrailNameList.parse));
          include_shadow_trails =
            (Util.option_bind (Xml.member "includeShadowTrails" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.include_shadow_trails
              (fun f ->
                 Query.Pair ("includeShadowTrails", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("trailNameList.member",
                  (TrailNameList.to_query v.trail_name_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.include_shadow_trails
              (fun f -> ("include_shadow_trails", (Boolean.to_json f)));
           Some
             ("trail_name_list", (TrailNameList.to_json v.trail_name_list))])
    let of_json j =
      {
        trail_name_list =
          (TrailNameList.of_json
             (Util.of_option_exn (Json.lookup j "trail_name_list")));
        include_shadow_trails =
          (Util.option_map (Json.lookup j "include_shadow_trails")
             Boolean.of_json)
      }
  end
module ListPublicKeysRequest =
  struct
    type t =
      {
      start_time: DateTime.t option ;
      end_time: DateTime.t option ;
      next_token: String.t option }
    let make ?start_time  ?end_time  ?next_token  () =
      { start_time; end_time; next_token }
    let parse xml =
      Some
        {
          start_time =
            (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse);
          end_time =
            (Util.option_bind (Xml.member "EndTime" xml) DateTime.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.end_time
             (fun f -> Query.Pair ("EndTime", (DateTime.to_query f)));
           Util.option_map v.start_time
             (fun f -> Query.Pair ("StartTime", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.end_time
             (fun f -> ("end_time", (DateTime.to_json f)));
           Util.option_map v.start_time
             (fun f -> ("start_time", (DateTime.to_json f)))])
    let of_json j =
      {
        start_time =
          (Util.option_map (Json.lookup j "start_time") DateTime.of_json);
        end_time =
          (Util.option_map (Json.lookup j "end_time") DateTime.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module InsufficientEncryptionPolicyException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module OperationNotPermittedException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CloudWatchLogsDeliveryUnavailableException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidMaxResultsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidParameterCombinationException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidTimeRangeException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CloudTrailAccessNotEnabledException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module UnsupportedOperationException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InsufficientSnsTopicPolicyException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidEventSelectorsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidHomeRegionException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InsufficientS3BucketPolicyException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module KmsKeyNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module TagsLimitExceededException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ResourceNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module LookupEventsResponse =
  struct
    type t = {
      events: EventsList.t ;
      next_token: String.t option }
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
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair ("Events.member", (EventsList.to_query v.events)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("events", (EventsList.to_json v.events))])
    let of_json j =
      {
        events =
          (EventsList.of_json (Util.of_option_exn (Json.lookup j "events")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module GetInsightSelectorsRequest =
  struct
    type t = {
      trail_name: String.t }
    let make ~trail_name  () = { trail_name }
    let parse xml =
      Some
        {
          trail_name =
            (Xml.required "TrailName"
               (Util.option_bind (Xml.member "TrailName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("TrailName", (String.to_query v.trail_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("trail_name", (String.to_json v.trail_name))])
    let of_json j =
      {
        trail_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "trail_name")))
      }
  end
module NotOrganizationMasterAccountException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RemoveTagsResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InsightNotEnabledException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidEventCategoryException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CloudTrailARNInvalidException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module OrganizationsNotInUseException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidCloudWatchLogsLogGroupArnException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidS3PrefixException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidNextTokenException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module MaximumNumberOfTrailsExceededException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GetEventSelectorsResponse =
  struct
    type t =
      {
      trail_a_r_n: String.t option ;
      event_selectors: EventSelectors.t }
    let make ?trail_a_r_n  ?(event_selectors= [])  () =
      { trail_a_r_n; event_selectors }
    let parse xml =
      Some
        {
          trail_a_r_n =
            (Util.option_bind (Xml.member "TrailARN" xml) String.parse);
          event_selectors =
            (Util.of_option []
               (Util.option_bind (Xml.member "EventSelectors" xml)
                  EventSelectors.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EventSelectors.member",
                   (EventSelectors.to_query v.event_selectors)));
           Util.option_map v.trail_a_r_n
             (fun f -> Query.Pair ("TrailARN", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("event_selectors", (EventSelectors.to_json v.event_selectors));
           Util.option_map v.trail_a_r_n
             (fun f -> ("trail_a_r_n", (String.to_json f)))])
    let of_json j =
      {
        trail_a_r_n =
          (Util.option_map (Json.lookup j "trail_a_r_n") String.of_json);
        event_selectors =
          (EventSelectors.of_json
             (Util.of_option_exn (Json.lookup j "event_selectors")))
      }
  end
module PutInsightSelectorsResponse =
  struct
    type t =
      {
      trail_a_r_n: String.t option ;
      insight_selectors: InsightSelectors.t }
    let make ?trail_a_r_n  ?(insight_selectors= [])  () =
      { trail_a_r_n; insight_selectors }
    let parse xml =
      Some
        {
          trail_a_r_n =
            (Util.option_bind (Xml.member "TrailARN" xml) String.parse);
          insight_selectors =
            (Util.of_option []
               (Util.option_bind (Xml.member "InsightSelectors" xml)
                  InsightSelectors.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("InsightSelectors.member",
                   (InsightSelectors.to_query v.insight_selectors)));
           Util.option_map v.trail_a_r_n
             (fun f -> Query.Pair ("TrailARN", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("insight_selectors",
                (InsightSelectors.to_json v.insight_selectors));
           Util.option_map v.trail_a_r_n
             (fun f -> ("trail_a_r_n", (String.to_json f)))])
    let of_json j =
      {
        trail_a_r_n =
          (Util.option_map (Json.lookup j "trail_a_r_n") String.of_json);
        insight_selectors =
          (InsightSelectors.of_json
             (Util.of_option_exn (Json.lookup j "insight_selectors")))
      }
  end
module ListPublicKeysResponse =
  struct
    type t = {
      public_key_list: PublicKeyList.t ;
      next_token: String.t option }
    let make ?(public_key_list= [])  ?next_token  () =
      { public_key_list; next_token }
    let parse xml =
      Some
        {
          public_key_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "PublicKeyList" xml)
                  PublicKeyList.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("PublicKeyList.member",
                  (PublicKeyList.to_query v.public_key_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("public_key_list", (PublicKeyList.to_json v.public_key_list))])
    let of_json j =
      {
        public_key_list =
          (PublicKeyList.of_json
             (Util.of_option_exn (Json.lookup j "public_key_list")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module InvalidSnsTopicNameException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GetTrailRequest =
  struct
    type t = {
      name: String.t }
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
module CreateTrailResponse =
  struct
    type t =
      {
      name: String.t option ;
      s3_bucket_name: String.t option ;
      s3_key_prefix: String.t option ;
      sns_topic_name: String.t option ;
      sns_topic_a_r_n: String.t option ;
      include_global_service_events: Boolean.t option ;
      is_multi_region_trail: Boolean.t option ;
      trail_a_r_n: String.t option ;
      log_file_validation_enabled: Boolean.t option ;
      cloud_watch_logs_log_group_arn: String.t option ;
      cloud_watch_logs_role_arn: String.t option ;
      kms_key_id: String.t option ;
      is_organization_trail: Boolean.t option }
    let make ?name  ?s3_bucket_name  ?s3_key_prefix  ?sns_topic_name 
      ?sns_topic_a_r_n  ?include_global_service_events 
      ?is_multi_region_trail  ?trail_a_r_n  ?log_file_validation_enabled 
      ?cloud_watch_logs_log_group_arn  ?cloud_watch_logs_role_arn 
      ?kms_key_id  ?is_organization_trail  () =
      {
        name;
        s3_bucket_name;
        s3_key_prefix;
        sns_topic_name;
        sns_topic_a_r_n;
        include_global_service_events;
        is_multi_region_trail;
        trail_a_r_n;
        log_file_validation_enabled;
        cloud_watch_logs_log_group_arn;
        cloud_watch_logs_role_arn;
        kms_key_id;
        is_organization_trail
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
          sns_topic_a_r_n =
            (Util.option_bind (Xml.member "SnsTopicARN" xml) String.parse);
          include_global_service_events =
            (Util.option_bind (Xml.member "IncludeGlobalServiceEvents" xml)
               Boolean.parse);
          is_multi_region_trail =
            (Util.option_bind (Xml.member "IsMultiRegionTrail" xml)
               Boolean.parse);
          trail_a_r_n =
            (Util.option_bind (Xml.member "TrailARN" xml) String.parse);
          log_file_validation_enabled =
            (Util.option_bind (Xml.member "LogFileValidationEnabled" xml)
               Boolean.parse);
          cloud_watch_logs_log_group_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
               String.parse);
          cloud_watch_logs_role_arn =
            (Util.option_bind (Xml.member "CloudWatchLogsRoleArn" xml)
               String.parse);
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse);
          is_organization_trail =
            (Util.option_bind (Xml.member "IsOrganizationTrail" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.is_organization_trail
              (fun f ->
                 Query.Pair ("IsOrganizationTrail", (Boolean.to_query f)));
           Util.option_map v.kms_key_id
             (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_role_arn
             (fun f ->
                Query.Pair ("CloudWatchLogsRoleArn", (String.to_query f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f ->
                Query.Pair ("CloudWatchLogsLogGroupArn", (String.to_query f)));
           Util.option_map v.log_file_validation_enabled
             (fun f ->
                Query.Pair ("LogFileValidationEnabled", (Boolean.to_query f)));
           Util.option_map v.trail_a_r_n
             (fun f -> Query.Pair ("TrailARN", (String.to_query f)));
           Util.option_map v.is_multi_region_trail
             (fun f ->
                Query.Pair ("IsMultiRegionTrail", (Boolean.to_query f)));
           Util.option_map v.include_global_service_events
             (fun f ->
                Query.Pair
                  ("IncludeGlobalServiceEvents", (Boolean.to_query f)));
           Util.option_map v.sns_topic_a_r_n
             (fun f -> Query.Pair ("SnsTopicARN", (String.to_query f)));
           Util.option_map v.sns_topic_name
             (fun f -> Query.Pair ("SnsTopicName", (String.to_query f)));
           Util.option_map v.s3_key_prefix
             (fun f -> Query.Pair ("S3KeyPrefix", (String.to_query f)));
           Util.option_map v.s3_bucket_name
             (fun f -> Query.Pair ("S3BucketName", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.is_organization_trail
              (fun f -> ("is_organization_trail", (Boolean.to_json f)));
           Util.option_map v.kms_key_id
             (fun f -> ("kms_key_id", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_role_arn
             (fun f -> ("cloud_watch_logs_role_arn", (String.to_json f)));
           Util.option_map v.cloud_watch_logs_log_group_arn
             (fun f -> ("cloud_watch_logs_log_group_arn", (String.to_json f)));
           Util.option_map v.log_file_validation_enabled
             (fun f -> ("log_file_validation_enabled", (Boolean.to_json f)));
           Util.option_map v.trail_a_r_n
             (fun f -> ("trail_a_r_n", (String.to_json f)));
           Util.option_map v.is_multi_region_trail
             (fun f -> ("is_multi_region_trail", (Boolean.to_json f)));
           Util.option_map v.include_global_service_events
             (fun f -> ("include_global_service_events", (Boolean.to_json f)));
           Util.option_map v.sns_topic_a_r_n
             (fun f -> ("sns_topic_a_r_n", (String.to_json f)));
           Util.option_map v.sns_topic_name
             (fun f -> ("sns_topic_name", (String.to_json f)));
           Util.option_map v.s3_key_prefix
             (fun f -> ("s3_key_prefix", (String.to_json f)));
           Util.option_map v.s3_bucket_name
             (fun f -> ("s3_bucket_name", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        s3_bucket_name =
          (Util.option_map (Json.lookup j "s3_bucket_name") String.of_json);
        s3_key_prefix =
          (Util.option_map (Json.lookup j "s3_key_prefix") String.of_json);
        sns_topic_name =
          (Util.option_map (Json.lookup j "sns_topic_name") String.of_json);
        sns_topic_a_r_n =
          (Util.option_map (Json.lookup j "sns_topic_a_r_n") String.of_json);
        include_global_service_events =
          (Util.option_map (Json.lookup j "include_global_service_events")
             Boolean.of_json);
        is_multi_region_trail =
          (Util.option_map (Json.lookup j "is_multi_region_trail")
             Boolean.of_json);
        trail_a_r_n =
          (Util.option_map (Json.lookup j "trail_a_r_n") String.of_json);
        log_file_validation_enabled =
          (Util.option_map (Json.lookup j "log_file_validation_enabled")
             Boolean.of_json);
        cloud_watch_logs_log_group_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_log_group_arn")
             String.of_json);
        cloud_watch_logs_role_arn =
          (Util.option_map (Json.lookup j "cloud_watch_logs_role_arn")
             String.of_json);
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json);
        is_organization_trail =
          (Util.option_map (Json.lookup j "is_organization_trail")
             Boolean.of_json)
      }
  end
module DescribeTrailsResponse =
  struct
    type t = {
      trail_list: TrailList.t }
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