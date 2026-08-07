open Aws.BaseTypes

type t =
  { trail_name : String.t option
  ; event_data_store : String.t option
  }

let make ?trail_name ?event_data_store () = { trail_name; event_data_store }

let parse xml =
  Some
    { trail_name = Aws.Util.option_bind (Aws.Xml.member "TrailName" xml) String.parse
    ; event_data_store =
        Aws.Util.option_bind (Aws.Xml.member "EventDataStore" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_data_store (fun f ->
             Aws.Query.Pair ("EventDataStore", String.to_query f))
       ; Aws.Util.option_map v.trail_name (fun f ->
             Aws.Query.Pair ("TrailName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_data_store (fun f ->
             "EventDataStore", String.to_json f)
       ; Aws.Util.option_map v.trail_name (fun f -> "TrailName", String.to_json f)
       ])

let of_json j =
  { trail_name = Aws.Util.option_map (Aws.Json.lookup j "TrailName") String.of_json
  ; event_data_store =
      Aws.Util.option_map (Aws.Json.lookup j "EventDataStore") String.of_json
  }
