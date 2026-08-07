open Aws.BaseTypes

type t =
  { d_b_shard_group_identifier : String.t option
  ; filters : FilterList.t
  ; marker : String.t option
  ; max_records : Integer.t option
  }

let make ?d_b_shard_group_identifier ?(filters = []) ?marker ?max_records () =
  { d_b_shard_group_identifier; filters; marker; max_records }

let parse xml =
  Some
    { d_b_shard_group_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBShardGroupIdentifier" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.d_b_shard_group_identifier (fun f ->
             Aws.Query.Pair ("DBShardGroupIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.d_b_shard_group_identifier (fun f ->
             "DBShardGroupIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_shard_group_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBShardGroupIdentifier") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  }
