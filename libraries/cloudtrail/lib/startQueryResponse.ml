open Aws.BaseTypes

type t =
  { query_id : String.t option
  ; event_data_store_owner_account_id : String.t option
  }

let make ?query_id ?event_data_store_owner_account_id () =
  { query_id; event_data_store_owner_account_id }

let parse xml =
  Some
    { query_id = Aws.Util.option_bind (Aws.Xml.member "QueryId" xml) String.parse
    ; event_data_store_owner_account_id =
        Aws.Util.option_bind
          (Aws.Xml.member "EventDataStoreOwnerAccountId" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_data_store_owner_account_id (fun f ->
             Aws.Query.Pair ("EventDataStoreOwnerAccountId", String.to_query f))
       ; Aws.Util.option_map v.query_id (fun f ->
             Aws.Query.Pair ("QueryId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_data_store_owner_account_id (fun f ->
             "EventDataStoreOwnerAccountId", String.to_json f)
       ; Aws.Util.option_map v.query_id (fun f -> "QueryId", String.to_json f)
       ])

let of_json j =
  { query_id = Aws.Util.option_map (Aws.Json.lookup j "QueryId") String.of_json
  ; event_data_store_owner_account_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "EventDataStoreOwnerAccountId")
        String.of_json
  }
