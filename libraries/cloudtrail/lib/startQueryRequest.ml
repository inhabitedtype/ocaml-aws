open Aws.BaseTypes

type t =
  { query_statement : String.t option
  ; delivery_s3_uri : String.t option
  ; query_alias : String.t option
  ; query_parameters : QueryParameters.t
  ; event_data_store_owner_account_id : String.t option
  }

let make
    ?query_statement
    ?delivery_s3_uri
    ?query_alias
    ?(query_parameters = [])
    ?event_data_store_owner_account_id
    () =
  { query_statement
  ; delivery_s3_uri
  ; query_alias
  ; query_parameters
  ; event_data_store_owner_account_id
  }

let parse xml =
  Some
    { query_statement =
        Aws.Util.option_bind (Aws.Xml.member "QueryStatement" xml) String.parse
    ; delivery_s3_uri =
        Aws.Util.option_bind (Aws.Xml.member "DeliveryS3Uri" xml) String.parse
    ; query_alias = Aws.Util.option_bind (Aws.Xml.member "QueryAlias" xml) String.parse
    ; query_parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "QueryParameters" xml)
             QueryParameters.parse)
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
       ; Some
           (Aws.Query.Pair
              ("QueryParameters.member", QueryParameters.to_query v.query_parameters))
       ; Aws.Util.option_map v.query_alias (fun f ->
             Aws.Query.Pair ("QueryAlias", String.to_query f))
       ; Aws.Util.option_map v.delivery_s3_uri (fun f ->
             Aws.Query.Pair ("DeliveryS3Uri", String.to_query f))
       ; Aws.Util.option_map v.query_statement (fun f ->
             Aws.Query.Pair ("QueryStatement", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_data_store_owner_account_id (fun f ->
             "EventDataStoreOwnerAccountId", String.to_json f)
       ; Some ("QueryParameters", QueryParameters.to_json v.query_parameters)
       ; Aws.Util.option_map v.query_alias (fun f -> "QueryAlias", String.to_json f)
       ; Aws.Util.option_map v.delivery_s3_uri (fun f ->
             "DeliveryS3Uri", String.to_json f)
       ; Aws.Util.option_map v.query_statement (fun f ->
             "QueryStatement", String.to_json f)
       ])

let of_json j =
  { query_statement =
      Aws.Util.option_map (Aws.Json.lookup j "QueryStatement") String.of_json
  ; delivery_s3_uri =
      Aws.Util.option_map (Aws.Json.lookup j "DeliveryS3Uri") String.of_json
  ; query_alias = Aws.Util.option_map (Aws.Json.lookup j "QueryAlias") String.of_json
  ; query_parameters =
      QueryParameters.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryParameters"))
  ; event_data_store_owner_account_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "EventDataStoreOwnerAccountId")
        String.of_json
  }
