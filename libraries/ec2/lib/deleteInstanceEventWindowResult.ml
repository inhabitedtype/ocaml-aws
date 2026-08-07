type t = { instance_event_window_state : InstanceEventWindowStateChange.t option }

let make ?instance_event_window_state () = { instance_event_window_state }

let parse xml =
  Some
    { instance_event_window_state =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceEventWindowState" xml)
          InstanceEventWindowStateChange.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_event_window_state (fun f ->
             Aws.Query.Pair
               ("InstanceEventWindowState", InstanceEventWindowStateChange.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_event_window_state (fun f ->
             "instanceEventWindowState", InstanceEventWindowStateChange.to_json f)
       ])

let of_json j =
  { instance_event_window_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceEventWindowState")
        InstanceEventWindowStateChange.of_json
  }
