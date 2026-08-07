open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t option
  ; d_b_snapshot_identifier : String.t option
  ; snapshot_type : String.t option
  ; filters : FilterList.t
  ; max_records : Integer.t option
  ; marker : String.t option
  ; dbi_resource_id : String.t option
  }

let make
    ?d_b_instance_identifier
    ?d_b_snapshot_identifier
    ?snapshot_type
    ?(filters = [])
    ?max_records
    ?marker
    ?dbi_resource_id
    () =
  { d_b_instance_identifier
  ; d_b_snapshot_identifier
  ; snapshot_type
  ; filters
  ; max_records
  ; marker
  ; dbi_resource_id
  }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse
    ; d_b_snapshot_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBSnapshotIdentifier" xml) String.parse
    ; snapshot_type =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotType" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; dbi_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DbiResourceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dbi_resource_id (fun f ->
             Aws.Query.Pair ("DbiResourceId", String.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.snapshot_type (fun f ->
             Aws.Query.Pair ("SnapshotType", String.to_query f))
       ; Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             Aws.Query.Pair ("DBSnapshotIdentifier", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("DBInstanceIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dbi_resource_id (fun f ->
             "DbiResourceId", String.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.snapshot_type (fun f -> "SnapshotType", String.to_json f)
       ; Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             "DBSnapshotIdentifier", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             "DBInstanceIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_instance_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceIdentifier") String.of_json
  ; d_b_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBSnapshotIdentifier") String.of_json
  ; snapshot_type = Aws.Util.option_map (Aws.Json.lookup j "SnapshotType") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; dbi_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DbiResourceId") String.of_json
  }
