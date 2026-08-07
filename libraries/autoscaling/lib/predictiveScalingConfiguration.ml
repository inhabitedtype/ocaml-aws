open Aws.BaseTypes

type t =
  { metric_specifications : PredictiveScalingMetricSpecifications.t
  ; mode : PredictiveScalingMode.t option
  ; scheduling_buffer_time : Integer.t option
  ; max_capacity_breach_behavior : PredictiveScalingMaxCapacityBreachBehavior.t option
  ; max_capacity_buffer : Integer.t option
  }

let make
    ~metric_specifications
    ?mode
    ?scheduling_buffer_time
    ?max_capacity_breach_behavior
    ?max_capacity_buffer
    () =
  { metric_specifications
  ; mode
  ; scheduling_buffer_time
  ; max_capacity_breach_behavior
  ; max_capacity_buffer
  }

let parse xml =
  Some
    { metric_specifications =
        Aws.Xml.required
          "MetricSpecifications"
          (Aws.Util.option_bind
             (Aws.Xml.member "MetricSpecifications" xml)
             PredictiveScalingMetricSpecifications.parse)
    ; mode = Aws.Util.option_bind (Aws.Xml.member "Mode" xml) PredictiveScalingMode.parse
    ; scheduling_buffer_time =
        Aws.Util.option_bind (Aws.Xml.member "SchedulingBufferTime" xml) Integer.parse
    ; max_capacity_breach_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "MaxCapacityBreachBehavior" xml)
          PredictiveScalingMaxCapacityBreachBehavior.parse
    ; max_capacity_buffer =
        Aws.Util.option_bind (Aws.Xml.member "MaxCapacityBuffer" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_capacity_buffer (fun f ->
             Aws.Query.Pair ("MaxCapacityBuffer", Integer.to_query f))
       ; Aws.Util.option_map v.max_capacity_breach_behavior (fun f ->
             Aws.Query.Pair
               ( "MaxCapacityBreachBehavior"
               , PredictiveScalingMaxCapacityBreachBehavior.to_query f ))
       ; Aws.Util.option_map v.scheduling_buffer_time (fun f ->
             Aws.Query.Pair ("SchedulingBufferTime", Integer.to_query f))
       ; Aws.Util.option_map v.mode (fun f ->
             Aws.Query.Pair ("Mode", PredictiveScalingMode.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "MetricSpecifications.member"
              , PredictiveScalingMetricSpecifications.to_query v.metric_specifications ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_capacity_buffer (fun f ->
             "MaxCapacityBuffer", Integer.to_json f)
       ; Aws.Util.option_map v.max_capacity_breach_behavior (fun f ->
             ( "MaxCapacityBreachBehavior"
             , PredictiveScalingMaxCapacityBreachBehavior.to_json f ))
       ; Aws.Util.option_map v.scheduling_buffer_time (fun f ->
             "SchedulingBufferTime", Integer.to_json f)
       ; Aws.Util.option_map v.mode (fun f -> "Mode", PredictiveScalingMode.to_json f)
       ; Some
           ( "MetricSpecifications"
           , PredictiveScalingMetricSpecifications.to_json v.metric_specifications )
       ])

let of_json j =
  { metric_specifications =
      PredictiveScalingMetricSpecifications.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricSpecifications"))
  ; mode = Aws.Util.option_map (Aws.Json.lookup j "Mode") PredictiveScalingMode.of_json
  ; scheduling_buffer_time =
      Aws.Util.option_map (Aws.Json.lookup j "SchedulingBufferTime") Integer.of_json
  ; max_capacity_breach_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "MaxCapacityBreachBehavior")
        PredictiveScalingMaxCapacityBreachBehavior.of_json
  ; max_capacity_buffer =
      Aws.Util.option_map (Aws.Json.lookup j "MaxCapacityBuffer") Integer.of_json
  }
