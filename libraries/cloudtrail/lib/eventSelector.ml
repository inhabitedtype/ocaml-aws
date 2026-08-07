open Aws.BaseTypes

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
        Aws.Util.option_bind (Aws.Xml.member "IncludeManagementEvents" xml) Boolean.parse
    ; data_resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "DataResources" xml) DataResources.parse)
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
              , ExcludeManagementEventSources.to_query v.exclude_management_event_sources
              ))
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
