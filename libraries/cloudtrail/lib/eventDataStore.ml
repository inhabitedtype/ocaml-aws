open Aws.BaseTypes

type t =
  { event_data_store_arn : String.t option
  ; name : String.t option
  ; termination_protection_enabled : Boolean.t option
  ; status : EventDataStoreStatus.t option
  ; advanced_event_selectors : AdvancedEventSelectors.t
  ; multi_region_enabled : Boolean.t option
  ; organization_enabled : Boolean.t option
  ; retention_period : Integer.t option
  ; created_timestamp : DateTime.t option
  ; updated_timestamp : DateTime.t option
  }

let make
    ?event_data_store_arn
    ?name
    ?termination_protection_enabled
    ?status
    ?(advanced_event_selectors = [])
    ?multi_region_enabled
    ?organization_enabled
    ?retention_period
    ?created_timestamp
    ?updated_timestamp
    () =
  { event_data_store_arn
  ; name
  ; termination_protection_enabled
  ; status
  ; advanced_event_selectors
  ; multi_region_enabled
  ; organization_enabled
  ; retention_period
  ; created_timestamp
  ; updated_timestamp
  }

let parse xml =
  Some
    { event_data_store_arn =
        Aws.Util.option_bind (Aws.Xml.member "EventDataStoreArn" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; termination_protection_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "TerminationProtectionEnabled" xml)
          Boolean.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) EventDataStoreStatus.parse
    ; advanced_event_selectors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AdvancedEventSelectors" xml)
             AdvancedEventSelectors.parse)
    ; multi_region_enabled =
        Aws.Util.option_bind (Aws.Xml.member "MultiRegionEnabled" xml) Boolean.parse
    ; organization_enabled =
        Aws.Util.option_bind (Aws.Xml.member "OrganizationEnabled" xml) Boolean.parse
    ; retention_period =
        Aws.Util.option_bind (Aws.Xml.member "RetentionPeriod" xml) Integer.parse
    ; created_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "CreatedTimestamp" xml) DateTime.parse
    ; updated_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "UpdatedTimestamp" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.updated_timestamp (fun f ->
             Aws.Query.Pair ("UpdatedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.created_timestamp (fun f ->
             Aws.Query.Pair ("CreatedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.retention_period (fun f ->
             Aws.Query.Pair ("RetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.organization_enabled (fun f ->
             Aws.Query.Pair ("OrganizationEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.multi_region_enabled (fun f ->
             Aws.Query.Pair ("MultiRegionEnabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AdvancedEventSelectors.member"
              , AdvancedEventSelectors.to_query v.advanced_event_selectors ))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", EventDataStoreStatus.to_query f))
       ; Aws.Util.option_map v.termination_protection_enabled (fun f ->
             Aws.Query.Pair ("TerminationProtectionEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.event_data_store_arn (fun f ->
             Aws.Query.Pair ("EventDataStoreArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.updated_timestamp (fun f ->
             "UpdatedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.created_timestamp (fun f ->
             "CreatedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.retention_period (fun f ->
             "RetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.organization_enabled (fun f ->
             "OrganizationEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.multi_region_enabled (fun f ->
             "MultiRegionEnabled", Boolean.to_json f)
       ; Some
           ( "AdvancedEventSelectors"
           , AdvancedEventSelectors.to_json v.advanced_event_selectors )
       ; Aws.Util.option_map v.status (fun f -> "Status", EventDataStoreStatus.to_json f)
       ; Aws.Util.option_map v.termination_protection_enabled (fun f ->
             "TerminationProtectionEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.event_data_store_arn (fun f ->
             "EventDataStoreArn", String.to_json f)
       ])

let of_json j =
  { event_data_store_arn =
      Aws.Util.option_map (Aws.Json.lookup j "EventDataStoreArn") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; termination_protection_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "TerminationProtectionEnabled")
        Boolean.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") EventDataStoreStatus.of_json
  ; advanced_event_selectors =
      AdvancedEventSelectors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdvancedEventSelectors"))
  ; multi_region_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "MultiRegionEnabled") Boolean.of_json
  ; organization_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "OrganizationEnabled") Boolean.of_json
  ; retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "RetentionPeriod") Integer.of_json
  ; created_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "CreatedTimestamp") DateTime.of_json
  ; updated_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "UpdatedTimestamp") DateTime.of_json
  }
