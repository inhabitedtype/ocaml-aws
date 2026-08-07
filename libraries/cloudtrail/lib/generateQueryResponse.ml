open Aws.BaseTypes

type t =
  { query_statement : String.t option
  ; query_alias : String.t option
  ; event_data_store_owner_account_id : String.t option
  }

let make ?query_statement ?query_alias ?event_data_store_owner_account_id () =
  { query_statement; query_alias; event_data_store_owner_account_id }

let parse xml =
  Some
    { query_statement =
        Aws.Util.option_bind (Aws.Xml.member "QueryStatement" xml) String.parse
    ; query_alias = Aws.Util.option_bind (Aws.Xml.member "QueryAlias" xml) String.parse
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
       ; Aws.Util.option_map v.query_alias (fun f ->
             Aws.Query.Pair ("QueryAlias", String.to_query f))
       ; Aws.Util.option_map v.query_statement (fun f ->
             Aws.Query.Pair ("QueryStatement", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_data_store_owner_account_id (fun f ->
             "EventDataStoreOwnerAccountId", String.to_json f)
       ; Aws.Util.option_map v.query_alias (fun f -> "QueryAlias", String.to_json f)
       ; Aws.Util.option_map v.query_statement (fun f ->
             "QueryStatement", String.to_json f)
       ])

let of_json j =
  { query_statement =
      Aws.Util.option_map (Aws.Json.lookup j "QueryStatement") String.of_json
  ; query_alias = Aws.Util.option_map (Aws.Json.lookup j "QueryAlias") String.of_json
  ; event_data_store_owner_account_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "EventDataStoreOwnerAccountId")
        String.of_json
  }
