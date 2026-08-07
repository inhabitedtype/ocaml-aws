open Aws.BaseTypes

type t =
  { alarm_name : String.t option
  ; alarm_arn : String.t option
  ; alarm_description : String.t option
  ; alarm_configuration_updated_timestamp : DateTime.t option
  ; actions_enabled : Boolean.t option
  ; o_k_actions : ResourceList.t
  ; alarm_actions : ResourceList.t
  ; insufficient_data_actions : ResourceList.t
  ; state_value : StateValue.t option
  ; state_reason : String.t option
  ; state_reason_data : String.t option
  ; state_updated_timestamp : DateTime.t option
  ; metric_name : String.t option
  ; namespace : String.t option
  ; statistic : Statistic.t option
  ; extended_statistic : String.t option
  ; dimensions : Dimensions.t
  ; period : Integer.t option
  ; unit : StandardUnit.t option
  ; evaluation_periods : Integer.t option
  ; datapoints_to_alarm : Integer.t option
  ; threshold : Double.t option
  ; comparison_operator : ComparisonOperator.t option
  ; treat_missing_data : String.t option
  ; evaluate_low_sample_count_percentile : String.t option
  ; metrics : MetricDataQueries.t
  ; threshold_metric_id : String.t option
  ; evaluation_state : EvaluationState.t option
  ; state_transitioned_timestamp : DateTime.t option
  ; evaluation_criteria : EvaluationCriteria.t option
  ; evaluation_interval : Integer.t option
  }

let make
    ?alarm_name
    ?alarm_arn
    ?alarm_description
    ?alarm_configuration_updated_timestamp
    ?actions_enabled
    ?(o_k_actions = [])
    ?(alarm_actions = [])
    ?(insufficient_data_actions = [])
    ?state_value
    ?state_reason
    ?state_reason_data
    ?state_updated_timestamp
    ?metric_name
    ?namespace
    ?statistic
    ?extended_statistic
    ?(dimensions = [])
    ?period
    ?unit
    ?evaluation_periods
    ?datapoints_to_alarm
    ?threshold
    ?comparison_operator
    ?treat_missing_data
    ?evaluate_low_sample_count_percentile
    ?(metrics = [])
    ?threshold_metric_id
    ?evaluation_state
    ?state_transitioned_timestamp
    ?evaluation_criteria
    ?evaluation_interval
    () =
  { alarm_name
  ; alarm_arn
  ; alarm_description
  ; alarm_configuration_updated_timestamp
  ; actions_enabled
  ; o_k_actions
  ; alarm_actions
  ; insufficient_data_actions
  ; state_value
  ; state_reason
  ; state_reason_data
  ; state_updated_timestamp
  ; metric_name
  ; namespace
  ; statistic
  ; extended_statistic
  ; dimensions
  ; period
  ; unit
  ; evaluation_periods
  ; datapoints_to_alarm
  ; threshold
  ; comparison_operator
  ; treat_missing_data
  ; evaluate_low_sample_count_percentile
  ; metrics
  ; threshold_metric_id
  ; evaluation_state
  ; state_transitioned_timestamp
  ; evaluation_criteria
  ; evaluation_interval
  }

let parse xml =
  Some
    { alarm_name = Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse
    ; alarm_arn = Aws.Util.option_bind (Aws.Xml.member "AlarmArn" xml) String.parse
    ; alarm_description =
        Aws.Util.option_bind (Aws.Xml.member "AlarmDescription" xml) String.parse
    ; alarm_configuration_updated_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "AlarmConfigurationUpdatedTimestamp" xml)
          DateTime.parse
    ; actions_enabled =
        Aws.Util.option_bind (Aws.Xml.member "ActionsEnabled" xml) Boolean.parse
    ; o_k_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "OKActions" xml) ResourceList.parse)
    ; alarm_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "AlarmActions" xml) ResourceList.parse)
    ; insufficient_data_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InsufficientDataActions" xml)
             ResourceList.parse)
    ; state_value =
        Aws.Util.option_bind (Aws.Xml.member "StateValue" xml) StateValue.parse
    ; state_reason = Aws.Util.option_bind (Aws.Xml.member "StateReason" xml) String.parse
    ; state_reason_data =
        Aws.Util.option_bind (Aws.Xml.member "StateReasonData" xml) String.parse
    ; state_updated_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "StateUpdatedTimestamp" xml) DateTime.parse
    ; metric_name = Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse
    ; namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
    ; statistic = Aws.Util.option_bind (Aws.Xml.member "Statistic" xml) Statistic.parse
    ; extended_statistic =
        Aws.Util.option_bind (Aws.Xml.member "ExtendedStatistic" xml) String.parse
    ; dimensions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
    ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse
    ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
    ; evaluation_periods =
        Aws.Util.option_bind (Aws.Xml.member "EvaluationPeriods" xml) Integer.parse
    ; datapoints_to_alarm =
        Aws.Util.option_bind (Aws.Xml.member "DatapointsToAlarm" xml) Integer.parse
    ; threshold = Aws.Util.option_bind (Aws.Xml.member "Threshold" xml) Double.parse
    ; comparison_operator =
        Aws.Util.option_bind
          (Aws.Xml.member "ComparisonOperator" xml)
          ComparisonOperator.parse
    ; treat_missing_data =
        Aws.Util.option_bind (Aws.Xml.member "TreatMissingData" xml) String.parse
    ; evaluate_low_sample_count_percentile =
        Aws.Util.option_bind
          (Aws.Xml.member "EvaluateLowSampleCountPercentile" xml)
          String.parse
    ; metrics =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Metrics" xml) MetricDataQueries.parse)
    ; threshold_metric_id =
        Aws.Util.option_bind (Aws.Xml.member "ThresholdMetricId" xml) String.parse
    ; evaluation_state =
        Aws.Util.option_bind (Aws.Xml.member "EvaluationState" xml) EvaluationState.parse
    ; state_transitioned_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "StateTransitionedTimestamp" xml)
          DateTime.parse
    ; evaluation_criteria =
        Aws.Util.option_bind
          (Aws.Xml.member "EvaluationCriteria" xml)
          EvaluationCriteria.parse
    ; evaluation_interval =
        Aws.Util.option_bind (Aws.Xml.member "EvaluationInterval" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.evaluation_interval (fun f ->
             Aws.Query.Pair ("EvaluationInterval", Integer.to_query f))
       ; Aws.Util.option_map v.evaluation_criteria (fun f ->
             Aws.Query.Pair ("EvaluationCriteria", EvaluationCriteria.to_query f))
       ; Aws.Util.option_map v.state_transitioned_timestamp (fun f ->
             Aws.Query.Pair ("StateTransitionedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.evaluation_state (fun f ->
             Aws.Query.Pair ("EvaluationState", EvaluationState.to_query f))
       ; Aws.Util.option_map v.threshold_metric_id (fun f ->
             Aws.Query.Pair ("ThresholdMetricId", String.to_query f))
       ; Some (Aws.Query.Pair ("Metrics.member", MetricDataQueries.to_query v.metrics))
       ; Aws.Util.option_map v.evaluate_low_sample_count_percentile (fun f ->
             Aws.Query.Pair ("EvaluateLowSampleCountPercentile", String.to_query f))
       ; Aws.Util.option_map v.treat_missing_data (fun f ->
             Aws.Query.Pair ("TreatMissingData", String.to_query f))
       ; Aws.Util.option_map v.comparison_operator (fun f ->
             Aws.Query.Pair ("ComparisonOperator", ComparisonOperator.to_query f))
       ; Aws.Util.option_map v.threshold (fun f ->
             Aws.Query.Pair ("Threshold", Double.to_query f))
       ; Aws.Util.option_map v.datapoints_to_alarm (fun f ->
             Aws.Query.Pair ("DatapointsToAlarm", Integer.to_query f))
       ; Aws.Util.option_map v.evaluation_periods (fun f ->
             Aws.Query.Pair ("EvaluationPeriods", Integer.to_query f))
       ; Aws.Util.option_map v.unit (fun f ->
             Aws.Query.Pair ("Unit", StandardUnit.to_query f))
       ; Aws.Util.option_map v.period (fun f ->
             Aws.Query.Pair ("Period", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
       ; Aws.Util.option_map v.extended_statistic (fun f ->
             Aws.Query.Pair ("ExtendedStatistic", String.to_query f))
       ; Aws.Util.option_map v.statistic (fun f ->
             Aws.Query.Pair ("Statistic", Statistic.to_query f))
       ; Aws.Util.option_map v.namespace (fun f ->
             Aws.Query.Pair ("Namespace", String.to_query f))
       ; Aws.Util.option_map v.metric_name (fun f ->
             Aws.Query.Pair ("MetricName", String.to_query f))
       ; Aws.Util.option_map v.state_updated_timestamp (fun f ->
             Aws.Query.Pair ("StateUpdatedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.state_reason_data (fun f ->
             Aws.Query.Pair ("StateReasonData", String.to_query f))
       ; Aws.Util.option_map v.state_reason (fun f ->
             Aws.Query.Pair ("StateReason", String.to_query f))
       ; Aws.Util.option_map v.state_value (fun f ->
             Aws.Query.Pair ("StateValue", StateValue.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InsufficientDataActions.member"
              , ResourceList.to_query v.insufficient_data_actions ))
       ; Some
           (Aws.Query.Pair ("AlarmActions.member", ResourceList.to_query v.alarm_actions))
       ; Some (Aws.Query.Pair ("OKActions.member", ResourceList.to_query v.o_k_actions))
       ; Aws.Util.option_map v.actions_enabled (fun f ->
             Aws.Query.Pair ("ActionsEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.alarm_configuration_updated_timestamp (fun f ->
             Aws.Query.Pair ("AlarmConfigurationUpdatedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.alarm_description (fun f ->
             Aws.Query.Pair ("AlarmDescription", String.to_query f))
       ; Aws.Util.option_map v.alarm_arn (fun f ->
             Aws.Query.Pair ("AlarmArn", String.to_query f))
       ; Aws.Util.option_map v.alarm_name (fun f ->
             Aws.Query.Pair ("AlarmName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.evaluation_interval (fun f ->
             "EvaluationInterval", Integer.to_json f)
       ; Aws.Util.option_map v.evaluation_criteria (fun f ->
             "EvaluationCriteria", EvaluationCriteria.to_json f)
       ; Aws.Util.option_map v.state_transitioned_timestamp (fun f ->
             "StateTransitionedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.evaluation_state (fun f ->
             "EvaluationState", EvaluationState.to_json f)
       ; Aws.Util.option_map v.threshold_metric_id (fun f ->
             "ThresholdMetricId", String.to_json f)
       ; Some ("Metrics", MetricDataQueries.to_json v.metrics)
       ; Aws.Util.option_map v.evaluate_low_sample_count_percentile (fun f ->
             "EvaluateLowSampleCountPercentile", String.to_json f)
       ; Aws.Util.option_map v.treat_missing_data (fun f ->
             "TreatMissingData", String.to_json f)
       ; Aws.Util.option_map v.comparison_operator (fun f ->
             "ComparisonOperator", ComparisonOperator.to_json f)
       ; Aws.Util.option_map v.threshold (fun f -> "Threshold", Double.to_json f)
       ; Aws.Util.option_map v.datapoints_to_alarm (fun f ->
             "DatapointsToAlarm", Integer.to_json f)
       ; Aws.Util.option_map v.evaluation_periods (fun f ->
             "EvaluationPeriods", Integer.to_json f)
       ; Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
       ; Aws.Util.option_map v.period (fun f -> "Period", Integer.to_json f)
       ; Some ("Dimensions", Dimensions.to_json v.dimensions)
       ; Aws.Util.option_map v.extended_statistic (fun f ->
             "ExtendedStatistic", String.to_json f)
       ; Aws.Util.option_map v.statistic (fun f -> "Statistic", Statistic.to_json f)
       ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
       ; Aws.Util.option_map v.metric_name (fun f -> "MetricName", String.to_json f)
       ; Aws.Util.option_map v.state_updated_timestamp (fun f ->
             "StateUpdatedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.state_reason_data (fun f ->
             "StateReasonData", String.to_json f)
       ; Aws.Util.option_map v.state_reason (fun f -> "StateReason", String.to_json f)
       ; Aws.Util.option_map v.state_value (fun f -> "StateValue", StateValue.to_json f)
       ; Some ("InsufficientDataActions", ResourceList.to_json v.insufficient_data_actions)
       ; Some ("AlarmActions", ResourceList.to_json v.alarm_actions)
       ; Some ("OKActions", ResourceList.to_json v.o_k_actions)
       ; Aws.Util.option_map v.actions_enabled (fun f ->
             "ActionsEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.alarm_configuration_updated_timestamp (fun f ->
             "AlarmConfigurationUpdatedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.alarm_description (fun f ->
             "AlarmDescription", String.to_json f)
       ; Aws.Util.option_map v.alarm_arn (fun f -> "AlarmArn", String.to_json f)
       ; Aws.Util.option_map v.alarm_name (fun f -> "AlarmName", String.to_json f)
       ])

let of_json j =
  { alarm_name = Aws.Util.option_map (Aws.Json.lookup j "AlarmName") String.of_json
  ; alarm_arn = Aws.Util.option_map (Aws.Json.lookup j "AlarmArn") String.of_json
  ; alarm_description =
      Aws.Util.option_map (Aws.Json.lookup j "AlarmDescription") String.of_json
  ; alarm_configuration_updated_timestamp =
      Aws.Util.option_map
        (Aws.Json.lookup j "AlarmConfigurationUpdatedTimestamp")
        DateTime.of_json
  ; actions_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "ActionsEnabled") Boolean.of_json
  ; o_k_actions =
      ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OKActions"))
  ; alarm_actions =
      ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmActions"))
  ; insufficient_data_actions =
      ResourceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InsufficientDataActions"))
  ; state_value = Aws.Util.option_map (Aws.Json.lookup j "StateValue") StateValue.of_json
  ; state_reason = Aws.Util.option_map (Aws.Json.lookup j "StateReason") String.of_json
  ; state_reason_data =
      Aws.Util.option_map (Aws.Json.lookup j "StateReasonData") String.of_json
  ; state_updated_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "StateUpdatedTimestamp") DateTime.of_json
  ; metric_name = Aws.Util.option_map (Aws.Json.lookup j "MetricName") String.of_json
  ; namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
  ; statistic = Aws.Util.option_map (Aws.Json.lookup j "Statistic") Statistic.of_json
  ; extended_statistic =
      Aws.Util.option_map (Aws.Json.lookup j "ExtendedStatistic") String.of_json
  ; dimensions =
      Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
  ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") Integer.of_json
  ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
  ; evaluation_periods =
      Aws.Util.option_map (Aws.Json.lookup j "EvaluationPeriods") Integer.of_json
  ; datapoints_to_alarm =
      Aws.Util.option_map (Aws.Json.lookup j "DatapointsToAlarm") Integer.of_json
  ; threshold = Aws.Util.option_map (Aws.Json.lookup j "Threshold") Double.of_json
  ; comparison_operator =
      Aws.Util.option_map
        (Aws.Json.lookup j "ComparisonOperator")
        ComparisonOperator.of_json
  ; treat_missing_data =
      Aws.Util.option_map (Aws.Json.lookup j "TreatMissingData") String.of_json
  ; evaluate_low_sample_count_percentile =
      Aws.Util.option_map
        (Aws.Json.lookup j "EvaluateLowSampleCountPercentile")
        String.of_json
  ; metrics =
      MetricDataQueries.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
  ; threshold_metric_id =
      Aws.Util.option_map (Aws.Json.lookup j "ThresholdMetricId") String.of_json
  ; evaluation_state =
      Aws.Util.option_map (Aws.Json.lookup j "EvaluationState") EvaluationState.of_json
  ; state_transitioned_timestamp =
      Aws.Util.option_map
        (Aws.Json.lookup j "StateTransitionedTimestamp")
        DateTime.of_json
  ; evaluation_criteria =
      Aws.Util.option_map
        (Aws.Json.lookup j "EvaluationCriteria")
        EvaluationCriteria.of_json
  ; evaluation_interval =
      Aws.Util.option_map (Aws.Json.lookup j "EvaluationInterval") Integer.of_json
  }
