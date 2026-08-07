open Aws.BaseTypes

type t =
  { d_b_cluster_identifier : String.t option
  ; d_b_cluster_snapshot_identifier : String.t option
  ; snapshot_type : String.t option
  ; filters : FilterList.t
  ; max_records : Integer.t option
  ; marker : String.t option
  ; include_shared : Boolean.t option
  ; include_public : Boolean.t option
  ; db_cluster_resource_id : String.t option
  }

let make
    ?d_b_cluster_identifier
    ?d_b_cluster_snapshot_identifier
    ?snapshot_type
    ?(filters = [])
    ?max_records
    ?marker
    ?include_shared
    ?include_public
    ?db_cluster_resource_id
    () =
  { d_b_cluster_identifier
  ; d_b_cluster_snapshot_identifier
  ; snapshot_type
  ; filters
  ; max_records
  ; marker
  ; include_shared
  ; include_public
  ; db_cluster_resource_id
  }

let parse xml =
  Some
    { d_b_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse
    ; d_b_cluster_snapshot_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterSnapshotIdentifier" xml)
          String.parse
    ; snapshot_type =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotType" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; include_shared =
        Aws.Util.option_bind (Aws.Xml.member "IncludeShared" xml) Boolean.parse
    ; include_public =
        Aws.Util.option_bind (Aws.Xml.member "IncludePublic" xml) Boolean.parse
    ; db_cluster_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DbClusterResourceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.db_cluster_resource_id (fun f ->
             Aws.Query.Pair ("DbClusterResourceId", String.to_query f))
       ; Aws.Util.option_map v.include_public (fun f ->
             Aws.Query.Pair ("IncludePublic", Boolean.to_query f))
       ; Aws.Util.option_map v.include_shared (fun f ->
             Aws.Query.Pair ("IncludeShared", Boolean.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.snapshot_type (fun f ->
             Aws.Query.Pair ("SnapshotType", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
             Aws.Query.Pair ("DBClusterSnapshotIdentifier", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("DBClusterIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.db_cluster_resource_id (fun f ->
             "DbClusterResourceId", String.to_json f)
       ; Aws.Util.option_map v.include_public (fun f ->
             "IncludePublic", Boolean.to_json f)
       ; Aws.Util.option_map v.include_shared (fun f ->
             "IncludeShared", Boolean.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.snapshot_type (fun f -> "SnapshotType", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
             "DBClusterSnapshotIdentifier", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             "DBClusterIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterIdentifier") String.of_json
  ; d_b_cluster_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterSnapshotIdentifier") String.of_json
  ; snapshot_type = Aws.Util.option_map (Aws.Json.lookup j "SnapshotType") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; include_shared =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeShared") Boolean.of_json
  ; include_public =
      Aws.Util.option_map (Aws.Json.lookup j "IncludePublic") Boolean.of_json
  ; db_cluster_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DbClusterResourceId") String.of_json
  }
