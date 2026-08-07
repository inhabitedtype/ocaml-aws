open Aws.BaseTypes

type t =
  { marker : String.t option
  ; events : EventList.t
  }

let make ?marker ?(events = []) () = { marker; events }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; events =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Events" xml) EventList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Events.member", EventList.to_query v.events))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Events", EventList.to_json v.events)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; events = EventList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Events"))
  }
