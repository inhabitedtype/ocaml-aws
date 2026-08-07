open Aws.BaseTypes

type t =
  { event_data_stores : EventDataStores.t
  ; next_token : String.t option
  }

let make ?(event_data_stores = []) ?next_token () = { event_data_stores; next_token }

let parse xml =
  Some
    { event_data_stores =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EventDataStores" xml)
             EventDataStores.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("EventDataStores.member", EventDataStores.to_query v.event_data_stores))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("EventDataStores", EventDataStores.to_json v.event_data_stores)
       ])

let of_json j =
  { event_data_stores =
      EventDataStores.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventDataStores"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
