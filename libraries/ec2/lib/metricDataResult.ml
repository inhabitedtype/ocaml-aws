open Aws.BaseTypes

type t =
  { dimension : CapacityManagerDimension.t option
  ; timestamp : DateTime.t option
  ; metric_values : MetricValueSet.t
  }

let make ?dimension ?timestamp ?(metric_values = []) () =
  { dimension; timestamp; metric_values }

let parse xml =
  Some
    { dimension =
        Aws.Util.option_bind
          (Aws.Xml.member "dimension" xml)
          CapacityManagerDimension.parse
    ; timestamp = Aws.Util.option_bind (Aws.Xml.member "timestamp" xml) DateTime.parse
    ; metric_values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "metricValueSet" xml)
             MetricValueSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MetricValueSet", MetricValueSet.to_query v.metric_values))
       ; Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.dimension (fun f ->
             Aws.Query.Pair ("Dimension", CapacityManagerDimension.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("metricValueSet", MetricValueSet.to_json v.metric_values)
       ; Aws.Util.option_map v.timestamp (fun f -> "timestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.dimension (fun f ->
             "dimension", CapacityManagerDimension.to_json f)
       ])

let of_json j =
  { dimension =
      Aws.Util.option_map (Aws.Json.lookup j "dimension") CapacityManagerDimension.of_json
  ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "timestamp") DateTime.of_json
  ; metric_values =
      MetricValueSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "metricValueSet"))
  }
