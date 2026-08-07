open Aws.BaseTypes

type t =
  { global_cluster_identifier : String.t option
  ; filters : FilterList.t
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make ?global_cluster_identifier ?(filters = []) ?max_records ?marker () =
  { global_cluster_identifier; filters; max_records; marker }

let parse xml =
  Some
    { global_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "GlobalClusterIdentifier" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.global_cluster_identifier (fun f ->
             Aws.Query.Pair ("GlobalClusterIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.global_cluster_identifier (fun f ->
             "GlobalClusterIdentifier", String.to_json f)
       ])

let of_json j =
  { global_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalClusterIdentifier") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
