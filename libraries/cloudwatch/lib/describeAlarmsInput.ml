open Aws.BaseTypes

type t =
  { alarm_names : AlarmNames.t
  ; alarm_name_prefix : String.t option
  ; alarm_types : AlarmTypes.t
  ; children_of_alarm_name : String.t option
  ; parents_of_alarm_name : String.t option
  ; state_value : StateValue.t option
  ; action_prefix : String.t option
  ; max_records : Integer.t option
  ; next_token : String.t option
  }

let make
    ?(alarm_names = [])
    ?alarm_name_prefix
    ?(alarm_types = [])
    ?children_of_alarm_name
    ?parents_of_alarm_name
    ?state_value
    ?action_prefix
    ?max_records
    ?next_token
    () =
  { alarm_names
  ; alarm_name_prefix
  ; alarm_types
  ; children_of_alarm_name
  ; parents_of_alarm_name
  ; state_value
  ; action_prefix
  ; max_records
  ; next_token
  }

let parse xml =
  Some
    { alarm_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "AlarmNames" xml) AlarmNames.parse)
    ; alarm_name_prefix =
        Aws.Util.option_bind (Aws.Xml.member "AlarmNamePrefix" xml) String.parse
    ; alarm_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "AlarmTypes" xml) AlarmTypes.parse)
    ; children_of_alarm_name =
        Aws.Util.option_bind (Aws.Xml.member "ChildrenOfAlarmName" xml) String.parse
    ; parents_of_alarm_name =
        Aws.Util.option_bind (Aws.Xml.member "ParentsOfAlarmName" xml) String.parse
    ; state_value =
        Aws.Util.option_bind (Aws.Xml.member "StateValue" xml) StateValue.parse
    ; action_prefix =
        Aws.Util.option_bind (Aws.Xml.member "ActionPrefix" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.action_prefix (fun f ->
             Aws.Query.Pair ("ActionPrefix", String.to_query f))
       ; Aws.Util.option_map v.state_value (fun f ->
             Aws.Query.Pair ("StateValue", StateValue.to_query f))
       ; Aws.Util.option_map v.parents_of_alarm_name (fun f ->
             Aws.Query.Pair ("ParentsOfAlarmName", String.to_query f))
       ; Aws.Util.option_map v.children_of_alarm_name (fun f ->
             Aws.Query.Pair ("ChildrenOfAlarmName", String.to_query f))
       ; Some (Aws.Query.Pair ("AlarmTypes.member", AlarmTypes.to_query v.alarm_types))
       ; Aws.Util.option_map v.alarm_name_prefix (fun f ->
             Aws.Query.Pair ("AlarmNamePrefix", String.to_query f))
       ; Some (Aws.Query.Pair ("AlarmNames.member", AlarmNames.to_query v.alarm_names))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.action_prefix (fun f -> "ActionPrefix", String.to_json f)
       ; Aws.Util.option_map v.state_value (fun f -> "StateValue", StateValue.to_json f)
       ; Aws.Util.option_map v.parents_of_alarm_name (fun f ->
             "ParentsOfAlarmName", String.to_json f)
       ; Aws.Util.option_map v.children_of_alarm_name (fun f ->
             "ChildrenOfAlarmName", String.to_json f)
       ; Some ("AlarmTypes", AlarmTypes.to_json v.alarm_types)
       ; Aws.Util.option_map v.alarm_name_prefix (fun f ->
             "AlarmNamePrefix", String.to_json f)
       ; Some ("AlarmNames", AlarmNames.to_json v.alarm_names)
       ])

let of_json j =
  { alarm_names =
      AlarmNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmNames"))
  ; alarm_name_prefix =
      Aws.Util.option_map (Aws.Json.lookup j "AlarmNamePrefix") String.of_json
  ; alarm_types =
      AlarmTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmTypes"))
  ; children_of_alarm_name =
      Aws.Util.option_map (Aws.Json.lookup j "ChildrenOfAlarmName") String.of_json
  ; parents_of_alarm_name =
      Aws.Util.option_map (Aws.Json.lookup j "ParentsOfAlarmName") String.of_json
  ; state_value = Aws.Util.option_map (Aws.Json.lookup j "StateValue") StateValue.of_json
  ; action_prefix = Aws.Util.option_map (Aws.Json.lookup j "ActionPrefix") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
