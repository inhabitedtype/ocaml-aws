type t = { alarm_names : AlarmNames.t }

let make ~alarm_names () = { alarm_names }

let parse xml =
  Some
    { alarm_names =
        Aws.Xml.required
          "AlarmNames"
          (Aws.Util.option_bind (Aws.Xml.member "AlarmNames" xml) AlarmNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("AlarmNames.member", AlarmNames.to_query v.alarm_names)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("AlarmNames", AlarmNames.to_json v.alarm_names) ])

let of_json j =
  { alarm_names =
      AlarmNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmNames"))
  }
