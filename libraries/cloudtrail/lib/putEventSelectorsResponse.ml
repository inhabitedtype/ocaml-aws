open Aws.BaseTypes

type t =
  { trail_a_r_n : String.t option
  ; event_selectors : EventSelectors.t
  ; advanced_event_selectors : AdvancedEventSelectors.t
  }

let make ?trail_a_r_n ?(event_selectors = []) ?(advanced_event_selectors = []) () =
  { trail_a_r_n; event_selectors; advanced_event_selectors }

let parse xml =
  Some
    { trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
    ; event_selectors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EventSelectors" xml)
             EventSelectors.parse)
    ; advanced_event_selectors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AdvancedEventSelectors" xml)
             AdvancedEventSelectors.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AdvancedEventSelectors.member"
              , AdvancedEventSelectors.to_query v.advanced_event_selectors ))
       ; Some
           (Aws.Query.Pair
              ("EventSelectors.member", EventSelectors.to_query v.event_selectors))
       ; Aws.Util.option_map v.trail_a_r_n (fun f ->
             Aws.Query.Pair ("TrailARN", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "AdvancedEventSelectors"
           , AdvancedEventSelectors.to_json v.advanced_event_selectors )
       ; Some ("EventSelectors", EventSelectors.to_json v.event_selectors)
       ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
       ])

let of_json j =
  { trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
  ; event_selectors =
      EventSelectors.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EventSelectors"))
  ; advanced_event_selectors =
      AdvancedEventSelectors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdvancedEventSelectors"))
  }
