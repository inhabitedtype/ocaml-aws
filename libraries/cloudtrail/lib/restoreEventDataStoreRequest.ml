open Aws.BaseTypes

type t = { event_data_store : String.t }

let make ~event_data_store () = { event_data_store }

let parse xml =
  Some
    { event_data_store =
        Aws.Xml.required
          "EventDataStore"
          (Aws.Util.option_bind (Aws.Xml.member "EventDataStore" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("EventDataStore", String.to_query v.event_data_store)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("EventDataStore", String.to_json v.event_data_store) ])

let of_json j =
  { event_data_store =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EventDataStore"))
  }
