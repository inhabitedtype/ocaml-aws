open Aws.BaseTypes

type t =
  { metric_name : String.t option
  ; namespace : String.t option
  ; dimensions : MetricDimensions.t
  ; statistic : MetricStatistic.t option
  ; unit : String.t option
  ; period : Integer.t option
  ; metrics : TargetTrackingMetricDataQueries.t
  }

let make
    ?metric_name
    ?namespace
    ?(dimensions = [])
    ?statistic
    ?unit
    ?period
    ?(metrics = [])
    () =
  { metric_name; namespace; dimensions; statistic; unit; period; metrics }

let parse xml =
  Some
    { metric_name = Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse
    ; namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
    ; dimensions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) MetricDimensions.parse)
    ; statistic =
        Aws.Util.option_bind (Aws.Xml.member "Statistic" xml) MetricStatistic.parse
    ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) String.parse
    ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse
    ; metrics =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Metrics" xml)
             TargetTrackingMetricDataQueries.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Metrics.member", TargetTrackingMetricDataQueries.to_query v.metrics))
       ; Aws.Util.option_map v.period (fun f ->
             Aws.Query.Pair ("Period", Integer.to_query f))
       ; Aws.Util.option_map v.unit (fun f -> Aws.Query.Pair ("Unit", String.to_query f))
       ; Aws.Util.option_map v.statistic (fun f ->
             Aws.Query.Pair ("Statistic", MetricStatistic.to_query f))
       ; Some
           (Aws.Query.Pair ("Dimensions.member", MetricDimensions.to_query v.dimensions))
       ; Aws.Util.option_map v.namespace (fun f ->
             Aws.Query.Pair ("Namespace", String.to_query f))
       ; Aws.Util.option_map v.metric_name (fun f ->
             Aws.Query.Pair ("MetricName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Metrics", TargetTrackingMetricDataQueries.to_json v.metrics)
       ; Aws.Util.option_map v.period (fun f -> "Period", Integer.to_json f)
       ; Aws.Util.option_map v.unit (fun f -> "Unit", String.to_json f)
       ; Aws.Util.option_map v.statistic (fun f -> "Statistic", MetricStatistic.to_json f)
       ; Some ("Dimensions", MetricDimensions.to_json v.dimensions)
       ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
       ; Aws.Util.option_map v.metric_name (fun f -> "MetricName", String.to_json f)
       ])

let of_json j =
  { metric_name = Aws.Util.option_map (Aws.Json.lookup j "MetricName") String.of_json
  ; namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
  ; dimensions =
      MetricDimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
  ; statistic =
      Aws.Util.option_map (Aws.Json.lookup j "Statistic") MetricStatistic.of_json
  ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") String.of_json
  ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") Integer.of_json
  ; metrics =
      TargetTrackingMetricDataQueries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
  }
