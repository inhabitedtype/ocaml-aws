open Aws.BaseTypes

type t =
  { event_data_stores : EventDataStoreList.t
  ; prompt : String.t
  }

let make ~event_data_stores ~prompt () = { event_data_stores; prompt }

let parse xml =
  Some
    { event_data_stores =
        Aws.Xml.required
          "EventDataStores"
          (Aws.Util.option_bind
             (Aws.Xml.member "EventDataStores" xml)
             EventDataStoreList.parse)
    ; prompt =
        Aws.Xml.required
          "Prompt"
          (Aws.Util.option_bind (Aws.Xml.member "Prompt" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Prompt", String.to_query v.prompt))
       ; Some
           (Aws.Query.Pair
              ("EventDataStores.member", EventDataStoreList.to_query v.event_data_stores))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Prompt", String.to_json v.prompt)
       ; Some ("EventDataStores", EventDataStoreList.to_json v.event_data_stores)
       ])

let of_json j =
  { event_data_stores =
      EventDataStoreList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventDataStores"))
  ; prompt = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Prompt"))
  }
