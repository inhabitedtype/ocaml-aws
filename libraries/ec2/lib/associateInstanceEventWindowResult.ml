type t = { instance_event_window : InstanceEventWindow.t option }

let make ?instance_event_window () = { instance_event_window }

let parse xml =
  Some
    { instance_event_window =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceEventWindow" xml)
          InstanceEventWindow.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_event_window (fun f ->
             Aws.Query.Pair ("InstanceEventWindow", InstanceEventWindow.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_event_window (fun f ->
             "instanceEventWindow", InstanceEventWindow.to_json f)
       ])

let of_json j =
  { instance_event_window =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceEventWindow")
        InstanceEventWindow.of_json
  }
