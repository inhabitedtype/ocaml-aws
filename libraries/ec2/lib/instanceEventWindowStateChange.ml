open Aws.BaseTypes

type t =
  { instance_event_window_id : String.t option
  ; state : InstanceEventWindowState.t option
  }

let make ?instance_event_window_id ?state () = { instance_event_window_id; state }

let parse xml =
  Some
    { instance_event_window_id =
        Aws.Util.option_bind (Aws.Xml.member "instanceEventWindowId" xml) String.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) InstanceEventWindowState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", InstanceEventWindowState.to_query f))
       ; Aws.Util.option_map v.instance_event_window_id (fun f ->
             Aws.Query.Pair ("InstanceEventWindowId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             "state", InstanceEventWindowState.to_json f)
       ; Aws.Util.option_map v.instance_event_window_id (fun f ->
             "instanceEventWindowId", String.to_json f)
       ])

let of_json j =
  { instance_event_window_id =
      Aws.Util.option_map (Aws.Json.lookup j "instanceEventWindowId") String.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") InstanceEventWindowState.of_json
  }
