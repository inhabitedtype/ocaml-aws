open Aws.BaseTypes

type t =
  { query_id : String.t
  ; query_status : QueryStatus.t
  ; event_data_store_owner_account_id : String.t option
  }

let make ~query_id ~query_status ?event_data_store_owner_account_id () =
  { query_id; query_status; event_data_store_owner_account_id }

let parse xml =
  Some
    { query_id =
        Aws.Xml.required
          "QueryId"
          (Aws.Util.option_bind (Aws.Xml.member "QueryId" xml) String.parse)
    ; query_status =
        Aws.Xml.required
          "QueryStatus"
          (Aws.Util.option_bind (Aws.Xml.member "QueryStatus" xml) QueryStatus.parse)
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
       ; Some (Aws.Query.Pair ("QueryStatus", QueryStatus.to_query v.query_status))
       ; Some (Aws.Query.Pair ("QueryId", String.to_query v.query_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_data_store_owner_account_id (fun f ->
             "EventDataStoreOwnerAccountId", String.to_json f)
       ; Some ("QueryStatus", QueryStatus.to_json v.query_status)
       ; Some ("QueryId", String.to_json v.query_id)
       ])

let of_json j =
  { query_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryId"))
  ; query_status =
      QueryStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryStatus"))
  ; event_data_store_owner_account_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "EventDataStoreOwnerAccountId")
        String.of_json
  }
