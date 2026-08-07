open Aws.BaseTypes

type t =
  { target_value : Double.t
  ; predefined_metric_pair_specification : PredictiveScalingPredefinedMetricPair.t option
  ; predefined_scaling_metric_specification :
      PredictiveScalingPredefinedScalingMetric.t option
  ; predefined_load_metric_specification : PredictiveScalingPredefinedLoadMetric.t option
  ; customized_scaling_metric_specification :
      PredictiveScalingCustomizedScalingMetric.t option
  ; customized_load_metric_specification : PredictiveScalingCustomizedLoadMetric.t option
  ; customized_capacity_metric_specification :
      PredictiveScalingCustomizedCapacityMetric.t option
  }

let make
    ~target_value
    ?predefined_metric_pair_specification
    ?predefined_scaling_metric_specification
    ?predefined_load_metric_specification
    ?customized_scaling_metric_specification
    ?customized_load_metric_specification
    ?customized_capacity_metric_specification
    () =
  { target_value
  ; predefined_metric_pair_specification
  ; predefined_scaling_metric_specification
  ; predefined_load_metric_specification
  ; customized_scaling_metric_specification
  ; customized_load_metric_specification
  ; customized_capacity_metric_specification
  }

let parse xml =
  Some
    { target_value =
        Aws.Xml.required
          "TargetValue"
          (Aws.Util.option_bind (Aws.Xml.member "TargetValue" xml) Double.parse)
    ; predefined_metric_pair_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "PredefinedMetricPairSpecification" xml)
          PredictiveScalingPredefinedMetricPair.parse
    ; predefined_scaling_metric_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "PredefinedScalingMetricSpecification" xml)
          PredictiveScalingPredefinedScalingMetric.parse
    ; predefined_load_metric_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "PredefinedLoadMetricSpecification" xml)
          PredictiveScalingPredefinedLoadMetric.parse
    ; customized_scaling_metric_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "CustomizedScalingMetricSpecification" xml)
          PredictiveScalingCustomizedScalingMetric.parse
    ; customized_load_metric_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "CustomizedLoadMetricSpecification" xml)
          PredictiveScalingCustomizedLoadMetric.parse
    ; customized_capacity_metric_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "CustomizedCapacityMetricSpecification" xml)
          PredictiveScalingCustomizedCapacityMetric.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.customized_capacity_metric_specification (fun f ->
             Aws.Query.Pair
               ( "CustomizedCapacityMetricSpecification"
               , PredictiveScalingCustomizedCapacityMetric.to_query f ))
       ; Aws.Util.option_map v.customized_load_metric_specification (fun f ->
             Aws.Query.Pair
               ( "CustomizedLoadMetricSpecification"
               , PredictiveScalingCustomizedLoadMetric.to_query f ))
       ; Aws.Util.option_map v.customized_scaling_metric_specification (fun f ->
             Aws.Query.Pair
               ( "CustomizedScalingMetricSpecification"
               , PredictiveScalingCustomizedScalingMetric.to_query f ))
       ; Aws.Util.option_map v.predefined_load_metric_specification (fun f ->
             Aws.Query.Pair
               ( "PredefinedLoadMetricSpecification"
               , PredictiveScalingPredefinedLoadMetric.to_query f ))
       ; Aws.Util.option_map v.predefined_scaling_metric_specification (fun f ->
             Aws.Query.Pair
               ( "PredefinedScalingMetricSpecification"
               , PredictiveScalingPredefinedScalingMetric.to_query f ))
       ; Aws.Util.option_map v.predefined_metric_pair_specification (fun f ->
             Aws.Query.Pair
               ( "PredefinedMetricPairSpecification"
               , PredictiveScalingPredefinedMetricPair.to_query f ))
       ; Some (Aws.Query.Pair ("TargetValue", Double.to_query v.target_value))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.customized_capacity_metric_specification (fun f ->
             ( "CustomizedCapacityMetricSpecification"
             , PredictiveScalingCustomizedCapacityMetric.to_json f ))
       ; Aws.Util.option_map v.customized_load_metric_specification (fun f ->
             ( "CustomizedLoadMetricSpecification"
             , PredictiveScalingCustomizedLoadMetric.to_json f ))
       ; Aws.Util.option_map v.customized_scaling_metric_specification (fun f ->
             ( "CustomizedScalingMetricSpecification"
             , PredictiveScalingCustomizedScalingMetric.to_json f ))
       ; Aws.Util.option_map v.predefined_load_metric_specification (fun f ->
             ( "PredefinedLoadMetricSpecification"
             , PredictiveScalingPredefinedLoadMetric.to_json f ))
       ; Aws.Util.option_map v.predefined_scaling_metric_specification (fun f ->
             ( "PredefinedScalingMetricSpecification"
             , PredictiveScalingPredefinedScalingMetric.to_json f ))
       ; Aws.Util.option_map v.predefined_metric_pair_specification (fun f ->
             ( "PredefinedMetricPairSpecification"
             , PredictiveScalingPredefinedMetricPair.to_json f ))
       ; Some ("TargetValue", Double.to_json v.target_value)
       ])

let of_json j =
  { target_value =
      Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetValue"))
  ; predefined_metric_pair_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "PredefinedMetricPairSpecification")
        PredictiveScalingPredefinedMetricPair.of_json
  ; predefined_scaling_metric_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "PredefinedScalingMetricSpecification")
        PredictiveScalingPredefinedScalingMetric.of_json
  ; predefined_load_metric_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "PredefinedLoadMetricSpecification")
        PredictiveScalingPredefinedLoadMetric.of_json
  ; customized_scaling_metric_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "CustomizedScalingMetricSpecification")
        PredictiveScalingCustomizedScalingMetric.of_json
  ; customized_load_metric_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "CustomizedLoadMetricSpecification")
        PredictiveScalingCustomizedLoadMetric.of_json
  ; customized_capacity_metric_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "CustomizedCapacityMetricSpecification")
        PredictiveScalingCustomizedCapacityMetric.of_json
  }
