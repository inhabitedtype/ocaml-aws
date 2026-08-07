open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t option
  ; policy_name : String.t option
  ; policy_a_r_n : String.t option
  ; policy_type : String.t option
  ; adjustment_type : String.t option
  ; min_adjustment_step : Integer.t option
  ; min_adjustment_magnitude : Integer.t option
  ; scaling_adjustment : Integer.t option
  ; cooldown : Integer.t option
  ; step_adjustments : StepAdjustments.t
  ; metric_aggregation_type : String.t option
  ; estimated_instance_warmup : Integer.t option
  ; alarms : Alarms.t
  ; target_tracking_configuration : TargetTrackingConfiguration.t option
  ; enabled : Boolean.t option
  ; predictive_scaling_configuration : PredictiveScalingConfiguration.t option
  }

let make
    ?auto_scaling_group_name
    ?policy_name
    ?policy_a_r_n
    ?policy_type
    ?adjustment_type
    ?min_adjustment_step
    ?min_adjustment_magnitude
    ?scaling_adjustment
    ?cooldown
    ?(step_adjustments = [])
    ?metric_aggregation_type
    ?estimated_instance_warmup
    ?(alarms = [])
    ?target_tracking_configuration
    ?enabled
    ?predictive_scaling_configuration
    () =
  { auto_scaling_group_name
  ; policy_name
  ; policy_a_r_n
  ; policy_type
  ; adjustment_type
  ; min_adjustment_step
  ; min_adjustment_magnitude
  ; scaling_adjustment
  ; cooldown
  ; step_adjustments
  ; metric_aggregation_type
  ; estimated_instance_warmup
  ; alarms
  ; target_tracking_configuration
  ; enabled
  ; predictive_scaling_configuration
  }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
    ; policy_name = Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse
    ; policy_a_r_n = Aws.Util.option_bind (Aws.Xml.member "PolicyARN" xml) String.parse
    ; policy_type = Aws.Util.option_bind (Aws.Xml.member "PolicyType" xml) String.parse
    ; adjustment_type =
        Aws.Util.option_bind (Aws.Xml.member "AdjustmentType" xml) String.parse
    ; min_adjustment_step =
        Aws.Util.option_bind (Aws.Xml.member "MinAdjustmentStep" xml) Integer.parse
    ; min_adjustment_magnitude =
        Aws.Util.option_bind (Aws.Xml.member "MinAdjustmentMagnitude" xml) Integer.parse
    ; scaling_adjustment =
        Aws.Util.option_bind (Aws.Xml.member "ScalingAdjustment" xml) Integer.parse
    ; cooldown = Aws.Util.option_bind (Aws.Xml.member "Cooldown" xml) Integer.parse
    ; step_adjustments =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "StepAdjustments" xml)
             StepAdjustments.parse)
    ; metric_aggregation_type =
        Aws.Util.option_bind (Aws.Xml.member "MetricAggregationType" xml) String.parse
    ; estimated_instance_warmup =
        Aws.Util.option_bind (Aws.Xml.member "EstimatedInstanceWarmup" xml) Integer.parse
    ; alarms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Alarms" xml) Alarms.parse)
    ; target_tracking_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetTrackingConfiguration" xml)
          TargetTrackingConfiguration.parse
    ; enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse
    ; predictive_scaling_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "PredictiveScalingConfiguration" xml)
          PredictiveScalingConfiguration.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.predictive_scaling_configuration (fun f ->
             Aws.Query.Pair
               ( "PredictiveScalingConfiguration"
               , PredictiveScalingConfiguration.to_query f ))
       ; Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ; Aws.Util.option_map v.target_tracking_configuration (fun f ->
             Aws.Query.Pair
               ("TargetTrackingConfiguration", TargetTrackingConfiguration.to_query f))
       ; Some (Aws.Query.Pair ("Alarms.member", Alarms.to_query v.alarms))
       ; Aws.Util.option_map v.estimated_instance_warmup (fun f ->
             Aws.Query.Pair ("EstimatedInstanceWarmup", Integer.to_query f))
       ; Aws.Util.option_map v.metric_aggregation_type (fun f ->
             Aws.Query.Pair ("MetricAggregationType", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("StepAdjustments.member", StepAdjustments.to_query v.step_adjustments))
       ; Aws.Util.option_map v.cooldown (fun f ->
             Aws.Query.Pair ("Cooldown", Integer.to_query f))
       ; Aws.Util.option_map v.scaling_adjustment (fun f ->
             Aws.Query.Pair ("ScalingAdjustment", Integer.to_query f))
       ; Aws.Util.option_map v.min_adjustment_magnitude (fun f ->
             Aws.Query.Pair ("MinAdjustmentMagnitude", Integer.to_query f))
       ; Aws.Util.option_map v.min_adjustment_step (fun f ->
             Aws.Query.Pair ("MinAdjustmentStep", Integer.to_query f))
       ; Aws.Util.option_map v.adjustment_type (fun f ->
             Aws.Query.Pair ("AdjustmentType", String.to_query f))
       ; Aws.Util.option_map v.policy_type (fun f ->
             Aws.Query.Pair ("PolicyType", String.to_query f))
       ; Aws.Util.option_map v.policy_a_r_n (fun f ->
             Aws.Query.Pair ("PolicyARN", String.to_query f))
       ; Aws.Util.option_map v.policy_name (fun f ->
             Aws.Query.Pair ("PolicyName", String.to_query f))
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.predictive_scaling_configuration (fun f ->
             "PredictiveScalingConfiguration", PredictiveScalingConfiguration.to_json f)
       ; Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f)
       ; Aws.Util.option_map v.target_tracking_configuration (fun f ->
             "TargetTrackingConfiguration", TargetTrackingConfiguration.to_json f)
       ; Some ("Alarms", Alarms.to_json v.alarms)
       ; Aws.Util.option_map v.estimated_instance_warmup (fun f ->
             "EstimatedInstanceWarmup", Integer.to_json f)
       ; Aws.Util.option_map v.metric_aggregation_type (fun f ->
             "MetricAggregationType", String.to_json f)
       ; Some ("StepAdjustments", StepAdjustments.to_json v.step_adjustments)
       ; Aws.Util.option_map v.cooldown (fun f -> "Cooldown", Integer.to_json f)
       ; Aws.Util.option_map v.scaling_adjustment (fun f ->
             "ScalingAdjustment", Integer.to_json f)
       ; Aws.Util.option_map v.min_adjustment_magnitude (fun f ->
             "MinAdjustmentMagnitude", Integer.to_json f)
       ; Aws.Util.option_map v.min_adjustment_step (fun f ->
             "MinAdjustmentStep", Integer.to_json f)
       ; Aws.Util.option_map v.adjustment_type (fun f ->
             "AdjustmentType", String.to_json f)
       ; Aws.Util.option_map v.policy_type (fun f -> "PolicyType", String.to_json f)
       ; Aws.Util.option_map v.policy_a_r_n (fun f -> "PolicyARN", String.to_json f)
       ; Aws.Util.option_map v.policy_name (fun f -> "PolicyName", String.to_json f)
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             "AutoScalingGroupName", String.to_json f)
       ])

let of_json j =
  { auto_scaling_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
  ; policy_name = Aws.Util.option_map (Aws.Json.lookup j "PolicyName") String.of_json
  ; policy_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "PolicyARN") String.of_json
  ; policy_type = Aws.Util.option_map (Aws.Json.lookup j "PolicyType") String.of_json
  ; adjustment_type =
      Aws.Util.option_map (Aws.Json.lookup j "AdjustmentType") String.of_json
  ; min_adjustment_step =
      Aws.Util.option_map (Aws.Json.lookup j "MinAdjustmentStep") Integer.of_json
  ; min_adjustment_magnitude =
      Aws.Util.option_map (Aws.Json.lookup j "MinAdjustmentMagnitude") Integer.of_json
  ; scaling_adjustment =
      Aws.Util.option_map (Aws.Json.lookup j "ScalingAdjustment") Integer.of_json
  ; cooldown = Aws.Util.option_map (Aws.Json.lookup j "Cooldown") Integer.of_json
  ; step_adjustments =
      StepAdjustments.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StepAdjustments"))
  ; metric_aggregation_type =
      Aws.Util.option_map (Aws.Json.lookup j "MetricAggregationType") String.of_json
  ; estimated_instance_warmup =
      Aws.Util.option_map (Aws.Json.lookup j "EstimatedInstanceWarmup") Integer.of_json
  ; alarms = Alarms.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Alarms"))
  ; target_tracking_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetTrackingConfiguration")
        TargetTrackingConfiguration.of_json
  ; enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json
  ; predictive_scaling_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "PredictiveScalingConfiguration")
        PredictiveScalingConfiguration.of_json
  }
