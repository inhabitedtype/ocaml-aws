open Aws.BaseTypes

type t =
  { event_data_store : String.t option
  ; query_id : String.t
  ; next_token : String.t option
  ; max_query_results : Integer.t option
  ; event_data_store_owner_account_id : String.t option
  }

let make
    ?event_data_store
    ~query_id
    ?next_token
    ?max_query_results
    ?event_data_store_owner_account_id
    () =
  { event_data_store
  ; query_id
  ; next_token
  ; max_query_results
  ; event_data_store_owner_account_id
  }

let parse xml =
  Some
    { event_data_store =
        Aws.Util.option_bind (Aws.Xml.member "EventDataStore" xml) String.parse
    ; query_id =
        Aws.Xml.required
          "QueryId"
          (Aws.Util.option_bind (Aws.Xml.member "QueryId" xml) String.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_query_results =
        Aws.Util.option_bind (Aws.Xml.member "MaxQueryResults" xml) Integer.parse
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
       ; Aws.Util.option_map v.max_query_results (fun f ->
             Aws.Query.Pair ("MaxQueryResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("QueryId", String.to_query v.query_id))
       ; Aws.Util.option_map v.event_data_store (fun f ->
             Aws.Query.Pair ("EventDataStore", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_data_store_owner_account_id (fun f ->
             "EventDataStoreOwnerAccountId", String.to_json f)
       ; Aws.Util.option_map v.max_query_results (fun f ->
             "MaxQueryResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("QueryId", String.to_json v.query_id)
       ; Aws.Util.option_map v.event_data_store (fun f ->
             "EventDataStore", String.to_json f)
       ])

let of_json j =
  { event_data_store =
      Aws.Util.option_map (Aws.Json.lookup j "EventDataStore") String.of_json
  ; query_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_query_results =
      Aws.Util.option_map (Aws.Json.lookup j "MaxQueryResults") Integer.of_json
  ; event_data_store_owner_account_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "EventDataStoreOwnerAccountId")
        String.of_json
  }
