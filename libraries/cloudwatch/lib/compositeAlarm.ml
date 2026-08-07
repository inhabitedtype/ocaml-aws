open Aws.BaseTypes

type t =
  { actions_enabled : Boolean.t option
  ; alarm_actions : ResourceList.t
  ; alarm_arn : String.t option
  ; alarm_configuration_updated_timestamp : DateTime.t option
  ; alarm_description : String.t option
  ; alarm_name : String.t option
  ; alarm_rule : String.t option
  ; insufficient_data_actions : ResourceList.t
  ; o_k_actions : ResourceList.t
  ; state_reason : String.t option
  ; state_reason_data : String.t option
  ; state_updated_timestamp : DateTime.t option
  ; state_value : StateValue.t option
  ; state_transitioned_timestamp : DateTime.t option
  ; actions_suppressed_by : ActionsSuppressedBy.t option
  ; actions_suppressed_reason : String.t option
  ; actions_suppressor : String.t option
  ; actions_suppressor_wait_period : Integer.t option
  ; actions_suppressor_extension_period : Integer.t option
  }

let make
    ?actions_enabled
    ?(alarm_actions = [])
    ?alarm_arn
    ?alarm_configuration_updated_timestamp
    ?alarm_description
    ?alarm_name
    ?alarm_rule
    ?(insufficient_data_actions = [])
    ?(o_k_actions = [])
    ?state_reason
    ?state_reason_data
    ?state_updated_timestamp
    ?state_value
    ?state_transitioned_timestamp
    ?actions_suppressed_by
    ?actions_suppressed_reason
    ?actions_suppressor
    ?actions_suppressor_wait_period
    ?actions_suppressor_extension_period
    () =
  { actions_enabled
  ; alarm_actions
  ; alarm_arn
  ; alarm_configuration_updated_timestamp
  ; alarm_description
  ; alarm_name
  ; alarm_rule
  ; insufficient_data_actions
  ; o_k_actions
  ; state_reason
  ; state_reason_data
  ; state_updated_timestamp
  ; state_value
  ; state_transitioned_timestamp
  ; actions_suppressed_by
  ; actions_suppressed_reason
  ; actions_suppressor
  ; actions_suppressor_wait_period
  ; actions_suppressor_extension_period
  }

let parse xml =
  Some
    { actions_enabled =
        Aws.Util.option_bind (Aws.Xml.member "ActionsEnabled" xml) Boolean.parse
    ; alarm_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "AlarmActions" xml) ResourceList.parse)
    ; alarm_arn = Aws.Util.option_bind (Aws.Xml.member "AlarmArn" xml) String.parse
    ; alarm_configuration_updated_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "AlarmConfigurationUpdatedTimestamp" xml)
          DateTime.parse
    ; alarm_description =
        Aws.Util.option_bind (Aws.Xml.member "AlarmDescription" xml) String.parse
    ; alarm_name = Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse
    ; alarm_rule = Aws.Util.option_bind (Aws.Xml.member "AlarmRule" xml) String.parse
    ; insufficient_data_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InsufficientDataActions" xml)
             ResourceList.parse)
    ; o_k_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "OKActions" xml) ResourceList.parse)
    ; state_reason = Aws.Util.option_bind (Aws.Xml.member "StateReason" xml) String.parse
    ; state_reason_data =
        Aws.Util.option_bind (Aws.Xml.member "StateReasonData" xml) String.parse
    ; state_updated_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "StateUpdatedTimestamp" xml) DateTime.parse
    ; state_value =
        Aws.Util.option_bind (Aws.Xml.member "StateValue" xml) StateValue.parse
    ; state_transitioned_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "StateTransitionedTimestamp" xml)
          DateTime.parse
    ; actions_suppressed_by =
        Aws.Util.option_bind
          (Aws.Xml.member "ActionsSuppressedBy" xml)
          ActionsSuppressedBy.parse
    ; actions_suppressed_reason =
        Aws.Util.option_bind (Aws.Xml.member "ActionsSuppressedReason" xml) String.parse
    ; actions_suppressor =
        Aws.Util.option_bind (Aws.Xml.member "ActionsSuppressor" xml) String.parse
    ; actions_suppressor_wait_period =
        Aws.Util.option_bind
          (Aws.Xml.member "ActionsSuppressorWaitPeriod" xml)
          Integer.parse
    ; actions_suppressor_extension_period =
        Aws.Util.option_bind
          (Aws.Xml.member "ActionsSuppressorExtensionPeriod" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.actions_suppressor_extension_period (fun f ->
             Aws.Query.Pair ("ActionsSuppressorExtensionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.actions_suppressor_wait_period (fun f ->
             Aws.Query.Pair ("ActionsSuppressorWaitPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.actions_suppressor (fun f ->
             Aws.Query.Pair ("ActionsSuppressor", String.to_query f))
       ; Aws.Util.option_map v.actions_suppressed_reason (fun f ->
             Aws.Query.Pair ("ActionsSuppressedReason", String.to_query f))
       ; Aws.Util.option_map v.actions_suppressed_by (fun f ->
             Aws.Query.Pair ("ActionsSuppressedBy", ActionsSuppressedBy.to_query f))
       ; Aws.Util.option_map v.state_transitioned_timestamp (fun f ->
             Aws.Query.Pair ("StateTransitionedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.state_value (fun f ->
             Aws.Query.Pair ("StateValue", StateValue.to_query f))
       ; Aws.Util.option_map v.state_updated_timestamp (fun f ->
             Aws.Query.Pair ("StateUpdatedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.state_reason_data (fun f ->
             Aws.Query.Pair ("StateReasonData", String.to_query f))
       ; Aws.Util.option_map v.state_reason (fun f ->
             Aws.Query.Pair ("StateReason", String.to_query f))
       ; Some (Aws.Query.Pair ("OKActions.member", ResourceList.to_query v.o_k_actions))
       ; Some
           (Aws.Query.Pair
              ( "InsufficientDataActions.member"
              , ResourceList.to_query v.insufficient_data_actions ))
       ; Aws.Util.option_map v.alarm_rule (fun f ->
             Aws.Query.Pair ("AlarmRule", String.to_query f))
       ; Aws.Util.option_map v.alarm_name (fun f ->
             Aws.Query.Pair ("AlarmName", String.to_query f))
       ; Aws.Util.option_map v.alarm_description (fun f ->
             Aws.Query.Pair ("AlarmDescription", String.to_query f))
       ; Aws.Util.option_map v.alarm_configuration_updated_timestamp (fun f ->
             Aws.Query.Pair ("AlarmConfigurationUpdatedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.alarm_arn (fun f ->
             Aws.Query.Pair ("AlarmArn", String.to_query f))
       ; Some
           (Aws.Query.Pair ("AlarmActions.member", ResourceList.to_query v.alarm_actions))
       ; Aws.Util.option_map v.actions_enabled (fun f ->
             Aws.Query.Pair ("ActionsEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.actions_suppressor_extension_period (fun f ->
             "ActionsSuppressorExtensionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.actions_suppressor_wait_period (fun f ->
             "ActionsSuppressorWaitPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.actions_suppressor (fun f ->
             "ActionsSuppressor", String.to_json f)
       ; Aws.Util.option_map v.actions_suppressed_reason (fun f ->
             "ActionsSuppressedReason", String.to_json f)
       ; Aws.Util.option_map v.actions_suppressed_by (fun f ->
             "ActionsSuppressedBy", ActionsSuppressedBy.to_json f)
       ; Aws.Util.option_map v.state_transitioned_timestamp (fun f ->
             "StateTransitionedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.state_value (fun f -> "StateValue", StateValue.to_json f)
       ; Aws.Util.option_map v.state_updated_timestamp (fun f ->
             "StateUpdatedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.state_reason_data (fun f ->
             "StateReasonData", String.to_json f)
       ; Aws.Util.option_map v.state_reason (fun f -> "StateReason", String.to_json f)
       ; Some ("OKActions", ResourceList.to_json v.o_k_actions)
       ; Some ("InsufficientDataActions", ResourceList.to_json v.insufficient_data_actions)
       ; Aws.Util.option_map v.alarm_rule (fun f -> "AlarmRule", String.to_json f)
       ; Aws.Util.option_map v.alarm_name (fun f -> "AlarmName", String.to_json f)
       ; Aws.Util.option_map v.alarm_description (fun f ->
             "AlarmDescription", String.to_json f)
       ; Aws.Util.option_map v.alarm_configuration_updated_timestamp (fun f ->
             "AlarmConfigurationUpdatedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.alarm_arn (fun f -> "AlarmArn", String.to_json f)
       ; Some ("AlarmActions", ResourceList.to_json v.alarm_actions)
       ; Aws.Util.option_map v.actions_enabled (fun f ->
             "ActionsEnabled", Boolean.to_json f)
       ])

let of_json j =
  { actions_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "ActionsEnabled") Boolean.of_json
  ; alarm_actions =
      ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmActions"))
  ; alarm_arn = Aws.Util.option_map (Aws.Json.lookup j "AlarmArn") String.of_json
  ; alarm_configuration_updated_timestamp =
      Aws.Util.option_map
        (Aws.Json.lookup j "AlarmConfigurationUpdatedTimestamp")
        DateTime.of_json
  ; alarm_description =
      Aws.Util.option_map (Aws.Json.lookup j "AlarmDescription") String.of_json
  ; alarm_name = Aws.Util.option_map (Aws.Json.lookup j "AlarmName") String.of_json
  ; alarm_rule = Aws.Util.option_map (Aws.Json.lookup j "AlarmRule") String.of_json
  ; insufficient_data_actions =
      ResourceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InsufficientDataActions"))
  ; o_k_actions =
      ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OKActions"))
  ; state_reason = Aws.Util.option_map (Aws.Json.lookup j "StateReason") String.of_json
  ; state_reason_data =
      Aws.Util.option_map (Aws.Json.lookup j "StateReasonData") String.of_json
  ; state_updated_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "StateUpdatedTimestamp") DateTime.of_json
  ; state_value = Aws.Util.option_map (Aws.Json.lookup j "StateValue") StateValue.of_json
  ; state_transitioned_timestamp =
      Aws.Util.option_map
        (Aws.Json.lookup j "StateTransitionedTimestamp")
        DateTime.of_json
  ; actions_suppressed_by =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActionsSuppressedBy")
        ActionsSuppressedBy.of_json
  ; actions_suppressed_reason =
      Aws.Util.option_map (Aws.Json.lookup j "ActionsSuppressedReason") String.of_json
  ; actions_suppressor =
      Aws.Util.option_map (Aws.Json.lookup j "ActionsSuppressor") String.of_json
  ; actions_suppressor_wait_period =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActionsSuppressorWaitPeriod")
        Integer.of_json
  ; actions_suppressor_extension_period =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActionsSuppressorExtensionPeriod")
        Integer.of_json
  }
