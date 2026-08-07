open Aws.BaseTypes

type t =
  { capacity_manager_status : CapacityManagerStatus.t option
  ; organizations_access : Boolean.t option
  ; data_export_count : Integer.t option
  ; ingestion_status : IngestionStatus.t option
  ; ingestion_status_message : String.t option
  ; earliest_datapoint_timestamp : DateTime.t option
  ; latest_datapoint_timestamp : DateTime.t option
  }

let make
    ?capacity_manager_status
    ?organizations_access
    ?data_export_count
    ?ingestion_status
    ?ingestion_status_message
    ?earliest_datapoint_timestamp
    ?latest_datapoint_timestamp
    () =
  { capacity_manager_status
  ; organizations_access
  ; data_export_count
  ; ingestion_status
  ; ingestion_status_message
  ; earliest_datapoint_timestamp
  ; latest_datapoint_timestamp
  }

let parse xml =
  Some
    { capacity_manager_status =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityManagerStatus" xml)
          CapacityManagerStatus.parse
    ; organizations_access =
        Aws.Util.option_bind (Aws.Xml.member "organizationsAccess" xml) Boolean.parse
    ; data_export_count =
        Aws.Util.option_bind (Aws.Xml.member "dataExportCount" xml) Integer.parse
    ; ingestion_status =
        Aws.Util.option_bind (Aws.Xml.member "ingestionStatus" xml) IngestionStatus.parse
    ; ingestion_status_message =
        Aws.Util.option_bind (Aws.Xml.member "ingestionStatusMessage" xml) String.parse
    ; earliest_datapoint_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "earliestDatapointTimestamp" xml)
          DateTime.parse
    ; latest_datapoint_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "latestDatapointTimestamp" xml)
          DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.latest_datapoint_timestamp (fun f ->
             Aws.Query.Pair ("LatestDatapointTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.earliest_datapoint_timestamp (fun f ->
             Aws.Query.Pair ("EarliestDatapointTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.ingestion_status_message (fun f ->
             Aws.Query.Pair ("IngestionStatusMessage", String.to_query f))
       ; Aws.Util.option_map v.ingestion_status (fun f ->
             Aws.Query.Pair ("IngestionStatus", IngestionStatus.to_query f))
       ; Aws.Util.option_map v.data_export_count (fun f ->
             Aws.Query.Pair ("DataExportCount", Integer.to_query f))
       ; Aws.Util.option_map v.organizations_access (fun f ->
             Aws.Query.Pair ("OrganizationsAccess", Boolean.to_query f))
       ; Aws.Util.option_map v.capacity_manager_status (fun f ->
             Aws.Query.Pair ("CapacityManagerStatus", CapacityManagerStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.latest_datapoint_timestamp (fun f ->
             "latestDatapointTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.earliest_datapoint_timestamp (fun f ->
             "earliestDatapointTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.ingestion_status_message (fun f ->
             "ingestionStatusMessage", String.to_json f)
       ; Aws.Util.option_map v.ingestion_status (fun f ->
             "ingestionStatus", IngestionStatus.to_json f)
       ; Aws.Util.option_map v.data_export_count (fun f ->
             "dataExportCount", Integer.to_json f)
       ; Aws.Util.option_map v.organizations_access (fun f ->
             "organizationsAccess", Boolean.to_json f)
       ; Aws.Util.option_map v.capacity_manager_status (fun f ->
             "capacityManagerStatus", CapacityManagerStatus.to_json f)
       ])

let of_json j =
  { capacity_manager_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityManagerStatus")
        CapacityManagerStatus.of_json
  ; organizations_access =
      Aws.Util.option_map (Aws.Json.lookup j "organizationsAccess") Boolean.of_json
  ; data_export_count =
      Aws.Util.option_map (Aws.Json.lookup j "dataExportCount") Integer.of_json
  ; ingestion_status =
      Aws.Util.option_map (Aws.Json.lookup j "ingestionStatus") IngestionStatus.of_json
  ; ingestion_status_message =
      Aws.Util.option_map (Aws.Json.lookup j "ingestionStatusMessage") String.of_json
  ; earliest_datapoint_timestamp =
      Aws.Util.option_map
        (Aws.Json.lookup j "earliestDatapointTimestamp")
        DateTime.of_json
  ; latest_datapoint_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "latestDatapointTimestamp") DateTime.of_json
  }
