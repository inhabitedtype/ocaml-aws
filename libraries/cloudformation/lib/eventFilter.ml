open Aws.BaseTypes

type t = { failed_events : Boolean.t option }

let make ?failed_events () = { failed_events }

let parse xml =
  Some
    { failed_events =
        Aws.Util.option_bind (Aws.Xml.member "FailedEvents" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failed_events (fun f ->
             Aws.Query.Pair ("FailedEvents", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failed_events (fun f -> "FailedEvents", Boolean.to_json f)
       ])

let of_json j =
  { failed_events = Aws.Util.option_map (Aws.Json.lookup j "FailedEvents") Boolean.of_json
  }
