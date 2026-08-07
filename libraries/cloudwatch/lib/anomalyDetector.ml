open Aws.BaseTypes

type t =
  { namespace : String.t option
  ; metric_name : String.t option
  ; dimensions : Dimensions.t
  ; stat : String.t option
  ; configuration : AnomalyDetectorConfiguration.t option
  ; state_value : AnomalyDetectorStateValue.t option
  ; metric_characteristics : MetricCharacteristics.t option
  ; single_metric_anomaly_detector : SingleMetricAnomalyDetector.t option
  ; metric_math_anomaly_detector : MetricMathAnomalyDetector.t option
  }

let make
    ?namespace
    ?metric_name
    ?(dimensions = [])
    ?stat
    ?configuration
    ?state_value
    ?metric_characteristics
    ?single_metric_anomaly_detector
    ?metric_math_anomaly_detector
    () =
  { namespace
  ; metric_name
  ; dimensions
  ; stat
  ; configuration
  ; state_value
  ; metric_characteristics
  ; single_metric_anomaly_detector
  ; metric_math_anomaly_detector
  }

let parse xml =
  Some
    { namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
    ; metric_name = Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse
    ; dimensions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
    ; stat = Aws.Util.option_bind (Aws.Xml.member "Stat" xml) String.parse
    ; configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "Configuration" xml)
          AnomalyDetectorConfiguration.parse
    ; state_value =
        Aws.Util.option_bind
          (Aws.Xml.member "StateValue" xml)
          AnomalyDetectorStateValue.parse
    ; metric_characteristics =
        Aws.Util.option_bind
          (Aws.Xml.member "MetricCharacteristics" xml)
          MetricCharacteristics.parse
    ; single_metric_anomaly_detector =
        Aws.Util.option_bind
          (Aws.Xml.member "SingleMetricAnomalyDetector" xml)
          SingleMetricAnomalyDetector.parse
    ; metric_math_anomaly_detector =
        Aws.Util.option_bind
          (Aws.Xml.member "MetricMathAnomalyDetector" xml)
          MetricMathAnomalyDetector.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric_math_anomaly_detector (fun f ->
             Aws.Query.Pair
               ("MetricMathAnomalyDetector", MetricMathAnomalyDetector.to_query f))
       ; Aws.Util.option_map v.single_metric_anomaly_detector (fun f ->
             Aws.Query.Pair
               ("SingleMetricAnomalyDetector", SingleMetricAnomalyDetector.to_query f))
       ; Aws.Util.option_map v.metric_characteristics (fun f ->
             Aws.Query.Pair ("MetricCharacteristics", MetricCharacteristics.to_query f))
       ; Aws.Util.option_map v.state_value (fun f ->
             Aws.Query.Pair ("StateValue", AnomalyDetectorStateValue.to_query f))
       ; Aws.Util.option_map v.configuration (fun f ->
             Aws.Query.Pair ("Configuration", AnomalyDetectorConfiguration.to_query f))
       ; Aws.Util.option_map v.stat (fun f -> Aws.Query.Pair ("Stat", String.to_query f))
       ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
       ; Aws.Util.option_map v.metric_name (fun f ->
             Aws.Query.Pair ("MetricName", String.to_query f))
       ; Aws.Util.option_map v.namespace (fun f ->
             Aws.Query.Pair ("Namespace", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric_math_anomaly_detector (fun f ->
             "MetricMathAnomalyDetector", MetricMathAnomalyDetector.to_json f)
       ; Aws.Util.option_map v.single_metric_anomaly_detector (fun f ->
             "SingleMetricAnomalyDetector", SingleMetricAnomalyDetector.to_json f)
       ; Aws.Util.option_map v.metric_characteristics (fun f ->
             "MetricCharacteristics", MetricCharacteristics.to_json f)
       ; Aws.Util.option_map v.state_value (fun f ->
             "StateValue", AnomalyDetectorStateValue.to_json f)
       ; Aws.Util.option_map v.configuration (fun f ->
             "Configuration", AnomalyDetectorConfiguration.to_json f)
       ; Aws.Util.option_map v.stat (fun f -> "Stat", String.to_json f)
       ; Some ("Dimensions", Dimensions.to_json v.dimensions)
       ; Aws.Util.option_map v.metric_name (fun f -> "MetricName", String.to_json f)
       ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
       ])

let of_json j =
  { namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
  ; metric_name = Aws.Util.option_map (Aws.Json.lookup j "MetricName") String.of_json
  ; dimensions =
      Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
  ; stat = Aws.Util.option_map (Aws.Json.lookup j "Stat") String.of_json
  ; configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "Configuration")
        AnomalyDetectorConfiguration.of_json
  ; state_value =
      Aws.Util.option_map
        (Aws.Json.lookup j "StateValue")
        AnomalyDetectorStateValue.of_json
  ; metric_characteristics =
      Aws.Util.option_map
        (Aws.Json.lookup j "MetricCharacteristics")
        MetricCharacteristics.of_json
  ; single_metric_anomaly_detector =
      Aws.Util.option_map
        (Aws.Json.lookup j "SingleMetricAnomalyDetector")
        SingleMetricAnomalyDetector.of_json
  ; metric_math_anomaly_detector =
      Aws.Util.option_map
        (Aws.Json.lookup j "MetricMathAnomalyDetector")
        MetricMathAnomalyDetector.of_json
  }
