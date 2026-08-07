type t = { alarm_names : MuteTargetAlarmNameList.t }

let make ~alarm_names () = { alarm_names }

let parse xml =
  Some
    { alarm_names =
        Aws.Xml.required
          "AlarmNames"
          (Aws.Util.option_bind
             (Aws.Xml.member "AlarmNames" xml)
             MuteTargetAlarmNameList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AlarmNames.member", MuteTargetAlarmNameList.to_query v.alarm_names))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AlarmNames", MuteTargetAlarmNameList.to_json v.alarm_names) ])

let of_json j =
  { alarm_names =
      MuteTargetAlarmNameList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmNames"))
  }
