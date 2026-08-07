type t = { event : InstanceStatusEvent.t option }

let make ?event () = { event }

let parse xml =
  Some
    { event = Aws.Util.option_bind (Aws.Xml.member "event" xml) InstanceStatusEvent.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event (fun f ->
             Aws.Query.Pair ("Event", InstanceStatusEvent.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event (fun f -> "event", InstanceStatusEvent.to_json f) ])

let of_json j =
  { event = Aws.Util.option_map (Aws.Json.lookup j "event") InstanceStatusEvent.of_json }
