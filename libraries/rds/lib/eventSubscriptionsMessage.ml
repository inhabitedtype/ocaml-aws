open Aws.BaseTypes

type t =
  { marker : String.t option
  ; event_subscriptions_list : EventSubscriptionsList.t
  }

let make ?marker ?(event_subscriptions_list = []) () =
  { marker; event_subscriptions_list }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; event_subscriptions_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EventSubscriptionsList" xml)
             EventSubscriptionsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "EventSubscriptionsList.member"
              , EventSubscriptionsList.to_query v.event_subscriptions_list ))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "EventSubscriptionsList"
           , EventSubscriptionsList.to_json v.event_subscriptions_list )
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; event_subscriptions_list =
      EventSubscriptionsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventSubscriptionsList"))
  }
