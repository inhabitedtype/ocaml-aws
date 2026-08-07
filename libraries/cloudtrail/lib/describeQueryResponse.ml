open Aws.BaseTypes

type t =
  { query_id : String.t option
  ; query_string : String.t option
  ; query_status : QueryStatus.t option
  ; query_statistics : QueryStatisticsForDescribeQuery.t option
  ; error_message : String.t option
  ; delivery_s3_uri : String.t option
  ; delivery_status : DeliveryStatus.t option
  ; prompt : String.t option
  ; event_data_store_owner_account_id : String.t option
  }

let make
    ?query_id
    ?query_string
    ?query_status
    ?query_statistics
    ?error_message
    ?delivery_s3_uri
    ?delivery_status
    ?prompt
    ?event_data_store_owner_account_id
    () =
  { query_id
  ; query_string
  ; query_status
  ; query_statistics
  ; error_message
  ; delivery_s3_uri
  ; delivery_status
  ; prompt
  ; event_data_store_owner_account_id
  }

let parse xml =
  Some
    { query_id = Aws.Util.option_bind (Aws.Xml.member "QueryId" xml) String.parse
    ; query_string = Aws.Util.option_bind (Aws.Xml.member "QueryString" xml) String.parse
    ; query_status =
        Aws.Util.option_bind (Aws.Xml.member "QueryStatus" xml) QueryStatus.parse
    ; query_statistics =
        Aws.Util.option_bind
          (Aws.Xml.member "QueryStatistics" xml)
          QueryStatisticsForDescribeQuery.parse
    ; error_message =
        Aws.Util.option_bind (Aws.Xml.member "ErrorMessage" xml) String.parse
    ; delivery_s3_uri =
        Aws.Util.option_bind (Aws.Xml.member "DeliveryS3Uri" xml) String.parse
    ; delivery_status =
        Aws.Util.option_bind (Aws.Xml.member "DeliveryStatus" xml) DeliveryStatus.parse
    ; prompt = Aws.Util.option_bind (Aws.Xml.member "Prompt" xml) String.parse
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
       ; Aws.Util.option_map v.prompt (fun f ->
             Aws.Query.Pair ("Prompt", String.to_query f))
       ; Aws.Util.option_map v.delivery_status (fun f ->
             Aws.Query.Pair ("DeliveryStatus", DeliveryStatus.to_query f))
       ; Aws.Util.option_map v.delivery_s3_uri (fun f ->
             Aws.Query.Pair ("DeliveryS3Uri", String.to_query f))
       ; Aws.Util.option_map v.error_message (fun f ->
             Aws.Query.Pair ("ErrorMessage", String.to_query f))
       ; Aws.Util.option_map v.query_statistics (fun f ->
             Aws.Query.Pair ("QueryStatistics", QueryStatisticsForDescribeQuery.to_query f))
       ; Aws.Util.option_map v.query_status (fun f ->
             Aws.Query.Pair ("QueryStatus", QueryStatus.to_query f))
       ; Aws.Util.option_map v.query_string (fun f ->
             Aws.Query.Pair ("QueryString", String.to_query f))
       ; Aws.Util.option_map v.query_id (fun f ->
             Aws.Query.Pair ("QueryId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_data_store_owner_account_id (fun f ->
             "EventDataStoreOwnerAccountId", String.to_json f)
       ; Aws.Util.option_map v.prompt (fun f -> "Prompt", String.to_json f)
       ; Aws.Util.option_map v.delivery_status (fun f ->
             "DeliveryStatus", DeliveryStatus.to_json f)
       ; Aws.Util.option_map v.delivery_s3_uri (fun f ->
             "DeliveryS3Uri", String.to_json f)
       ; Aws.Util.option_map v.error_message (fun f -> "ErrorMessage", String.to_json f)
       ; Aws.Util.option_map v.query_statistics (fun f ->
             "QueryStatistics", QueryStatisticsForDescribeQuery.to_json f)
       ; Aws.Util.option_map v.query_status (fun f ->
             "QueryStatus", QueryStatus.to_json f)
       ; Aws.Util.option_map v.query_string (fun f -> "QueryString", String.to_json f)
       ; Aws.Util.option_map v.query_id (fun f -> "QueryId", String.to_json f)
       ])

let of_json j =
  { query_id = Aws.Util.option_map (Aws.Json.lookup j "QueryId") String.of_json
  ; query_string = Aws.Util.option_map (Aws.Json.lookup j "QueryString") String.of_json
  ; query_status =
      Aws.Util.option_map (Aws.Json.lookup j "QueryStatus") QueryStatus.of_json
  ; query_statistics =
      Aws.Util.option_map
        (Aws.Json.lookup j "QueryStatistics")
        QueryStatisticsForDescribeQuery.of_json
  ; error_message = Aws.Util.option_map (Aws.Json.lookup j "ErrorMessage") String.of_json
  ; delivery_s3_uri =
      Aws.Util.option_map (Aws.Json.lookup j "DeliveryS3Uri") String.of_json
  ; delivery_status =
      Aws.Util.option_map (Aws.Json.lookup j "DeliveryStatus") DeliveryStatus.of_json
  ; prompt = Aws.Util.option_map (Aws.Json.lookup j "Prompt") String.of_json
  ; event_data_store_owner_account_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "EventDataStoreOwnerAccountId")
        String.of_json
  }
