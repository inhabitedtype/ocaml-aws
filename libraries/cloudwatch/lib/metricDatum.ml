open Aws.BaseTypes

type t =
  { metric_name : String.t
  ; dimensions : Dimensions.t
  ; timestamp : DateTime.t option
  ; value : Double.t option
  ; statistic_values : StatisticSet.t option
  ; values : Values.t
  ; counts : Counts.t
  ; unit : StandardUnit.t option
  ; storage_resolution : Integer.t option
  }

let make
    ~metric_name
    ?(dimensions = [])
    ?timestamp
    ?value
    ?statistic_values
    ?(values = [])
    ?(counts = [])
    ?unit
    ?storage_resolution
    () =
  { metric_name
  ; dimensions
  ; timestamp
  ; value
  ; statistic_values
  ; values
  ; counts
  ; unit
  ; storage_resolution
  }

let parse xml =
  Some
    { metric_name =
        Aws.Xml.required
          "MetricName"
          (Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse)
    ; dimensions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
    ; timestamp = Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) Double.parse
    ; statistic_values =
        Aws.Util.option_bind (Aws.Xml.member "StatisticValues" xml) StatisticSet.parse
    ; values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Values" xml) Values.parse)
    ; counts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Counts" xml) Counts.parse)
    ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
    ; storage_resolution =
        Aws.Util.option_bind (Aws.Xml.member "StorageResolution" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.storage_resolution (fun f ->
             Aws.Query.Pair ("StorageResolution", Integer.to_query f))
       ; Aws.Util.option_map v.unit (fun f ->
             Aws.Query.Pair ("Unit", StandardUnit.to_query f))
       ; Some (Aws.Query.Pair ("Counts.member", Counts.to_query v.counts))
       ; Some (Aws.Query.Pair ("Values.member", Values.to_query v.values))
       ; Aws.Util.option_map v.statistic_values (fun f ->
             Aws.Query.Pair ("StatisticValues", StatisticSet.to_query f))
       ; Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", Double.to_query f))
       ; Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
       ; Some (Aws.Query.Pair ("MetricName", String.to_query v.metric_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.storage_resolution (fun f ->
             "StorageResolution", Integer.to_json f)
       ; Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
       ; Some ("Counts", Counts.to_json v.counts)
       ; Some ("Values", Values.to_json v.values)
       ; Aws.Util.option_map v.statistic_values (fun f ->
             "StatisticValues", StatisticSet.to_json f)
       ; Aws.Util.option_map v.value (fun f -> "Value", Double.to_json f)
       ; Aws.Util.option_map v.timestamp (fun f -> "Timestamp", DateTime.to_json f)
       ; Some ("Dimensions", Dimensions.to_json v.dimensions)
       ; Some ("MetricName", String.to_json v.metric_name)
       ])

let of_json j =
  { metric_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
  ; dimensions =
      Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
  ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "Timestamp") DateTime.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") Double.of_json
  ; statistic_values =
      Aws.Util.option_map (Aws.Json.lookup j "StatisticValues") StatisticSet.of_json
  ; values = Values.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Values"))
  ; counts = Counts.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Counts"))
  ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
  ; storage_resolution =
      Aws.Util.option_map (Aws.Json.lookup j "StorageResolution") Integer.of_json
  }
