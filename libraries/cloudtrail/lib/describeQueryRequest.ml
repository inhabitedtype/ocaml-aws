open Aws.BaseTypes

type t =
  { event_data_store : String.t option
  ; query_id : String.t option
  ; query_alias : String.t option
  ; refresh_id : String.t option
  ; event_data_store_owner_account_id : String.t option
  }

let make
    ?event_data_store
    ?query_id
    ?query_alias
    ?refresh_id
    ?event_data_store_owner_account_id
    () =
  { event_data_store
  ; query_id
  ; query_alias
  ; refresh_id
  ; event_data_store_owner_account_id
  }

let parse xml =
  Some
    { event_data_store =
        Aws.Util.option_bind (Aws.Xml.member "EventDataStore" xml) String.parse
    ; query_id = Aws.Util.option_bind (Aws.Xml.member "QueryId" xml) String.parse
    ; query_alias = Aws.Util.option_bind (Aws.Xml.member "QueryAlias" xml) String.parse
    ; refresh_id = Aws.Util.option_bind (Aws.Xml.member "RefreshId" xml) String.parse
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
       ; Aws.Util.option_map v.refresh_id (fun f ->
             Aws.Query.Pair ("RefreshId", String.to_query f))
       ; Aws.Util.option_map v.query_alias (fun f ->
             Aws.Query.Pair ("QueryAlias", String.to_query f))
       ; Aws.Util.option_map v.query_id (fun f ->
             Aws.Query.Pair ("QueryId", String.to_query f))
       ; Aws.Util.option_map v.event_data_store (fun f ->
             Aws.Query.Pair ("EventDataStore", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_data_store_owner_account_id (fun f ->
             "EventDataStoreOwnerAccountId", String.to_json f)
       ; Aws.Util.option_map v.refresh_id (fun f -> "RefreshId", String.to_json f)
       ; Aws.Util.option_map v.query_alias (fun f -> "QueryAlias", String.to_json f)
       ; Aws.Util.option_map v.query_id (fun f -> "QueryId", String.to_json f)
       ; Aws.Util.option_map v.event_data_store (fun f ->
             "EventDataStore", String.to_json f)
       ])

let of_json j =
  { event_data_store =
      Aws.Util.option_map (Aws.Json.lookup j "EventDataStore") String.of_json
  ; query_id = Aws.Util.option_map (Aws.Json.lookup j "QueryId") String.of_json
  ; query_alias = Aws.Util.option_map (Aws.Json.lookup j "QueryAlias") String.of_json
  ; refresh_id = Aws.Util.option_map (Aws.Json.lookup j "RefreshId") String.of_json
  ; event_data_store_owner_account_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "EventDataStoreOwnerAccountId")
        String.of_json
  }
