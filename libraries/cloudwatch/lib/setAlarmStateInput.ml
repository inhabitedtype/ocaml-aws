open Aws.BaseTypes

type t =
  { alarm_name : String.t
  ; state_value : StateValue.t
  ; state_reason : String.t
  ; state_reason_data : String.t option
  }

let make ~alarm_name ~state_value ~state_reason ?state_reason_data () =
  { alarm_name; state_value; state_reason; state_reason_data }

let parse xml =
  Some
    { alarm_name =
        Aws.Xml.required
          "AlarmName"
          (Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse)
    ; state_value =
        Aws.Xml.required
          "StateValue"
          (Aws.Util.option_bind (Aws.Xml.member "StateValue" xml) StateValue.parse)
    ; state_reason =
        Aws.Xml.required
          "StateReason"
          (Aws.Util.option_bind (Aws.Xml.member "StateReason" xml) String.parse)
    ; state_reason_data =
        Aws.Util.option_bind (Aws.Xml.member "StateReasonData" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_reason_data (fun f ->
             Aws.Query.Pair ("StateReasonData", String.to_query f))
       ; Some (Aws.Query.Pair ("StateReason", String.to_query v.state_reason))
       ; Some (Aws.Query.Pair ("StateValue", StateValue.to_query v.state_value))
       ; Some (Aws.Query.Pair ("AlarmName", String.to_query v.alarm_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_reason_data (fun f ->
             "StateReasonData", String.to_json f)
       ; Some ("StateReason", String.to_json v.state_reason)
       ; Some ("StateValue", StateValue.to_json v.state_value)
       ; Some ("AlarmName", String.to_json v.alarm_name)
       ])

let of_json j =
  { alarm_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmName"))
  ; state_value =
      StateValue.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StateValue"))
  ; state_reason =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StateReason"))
  ; state_reason_data =
      Aws.Util.option_map (Aws.Json.lookup j "StateReasonData") String.of_json
  }
