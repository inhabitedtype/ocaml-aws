type t = { alarms : AlarmList.t }

let make ?(alarms = []) () = { alarms }

let parse xml =
  Some
    { alarms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Alarms" xml) AlarmList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Alarms.member", AlarmList.to_query v.alarms)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Alarms", AlarmList.to_json v.alarms) ])

let of_json j =
  { alarms = AlarmList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Alarms")) }
