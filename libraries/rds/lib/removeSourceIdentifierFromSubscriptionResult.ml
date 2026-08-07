type t = { event_subscription : EventSubscription.t option }

let make ?event_subscription () = { event_subscription }

let parse xml =
  Some
    { event_subscription =
        Aws.Util.option_bind
          (Aws.Xml.member "EventSubscription" xml)
          EventSubscription.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_subscription (fun f ->
             Aws.Query.Pair ("EventSubscription", EventSubscription.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_subscription (fun f ->
             "EventSubscription", EventSubscription.to_json f)
       ])

let of_json j =
  { event_subscription =
      Aws.Util.option_map
        (Aws.Json.lookup j "EventSubscription")
        EventSubscription.of_json
  }
