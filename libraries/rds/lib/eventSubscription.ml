open Aws.BaseTypes

type t =
  { customer_aws_id : String.t option
  ; cust_subscription_id : String.t option
  ; sns_topic_arn : String.t option
  ; status : String.t option
  ; subscription_creation_time : String.t option
  ; source_type : String.t option
  ; source_ids_list : SourceIdsList.t
  ; event_categories_list : EventCategoriesList.t
  ; enabled : Boolean.t option
  ; event_subscription_arn : String.t option
  }

let make
    ?customer_aws_id
    ?cust_subscription_id
    ?sns_topic_arn
    ?status
    ?subscription_creation_time
    ?source_type
    ?(source_ids_list = [])
    ?(event_categories_list = [])
    ?enabled
    ?event_subscription_arn
    () =
  { customer_aws_id
  ; cust_subscription_id
  ; sns_topic_arn
  ; status
  ; subscription_creation_time
  ; source_type
  ; source_ids_list
  ; event_categories_list
  ; enabled
  ; event_subscription_arn
  }

let parse xml =
  Some
    { customer_aws_id =
        Aws.Util.option_bind (Aws.Xml.member "CustomerAwsId" xml) String.parse
    ; cust_subscription_id =
        Aws.Util.option_bind (Aws.Xml.member "CustSubscriptionId" xml) String.parse
    ; sns_topic_arn = Aws.Util.option_bind (Aws.Xml.member "SnsTopicArn" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; subscription_creation_time =
        Aws.Util.option_bind (Aws.Xml.member "SubscriptionCreationTime" xml) String.parse
    ; source_type = Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) String.parse
    ; source_ids_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "SourceIdsList" xml) SourceIdsList.parse)
    ; event_categories_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EventCategoriesList" xml)
             EventCategoriesList.parse)
    ; enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse
    ; event_subscription_arn =
        Aws.Util.option_bind (Aws.Xml.member "EventSubscriptionArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_subscription_arn (fun f ->
             Aws.Query.Pair ("EventSubscriptionArn", String.to_query f))
       ; Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "EventCategoriesList.member"
              , EventCategoriesList.to_query v.event_categories_list ))
       ; Some
           (Aws.Query.Pair
              ("SourceIdsList.member", SourceIdsList.to_query v.source_ids_list))
       ; Aws.Util.option_map v.source_type (fun f ->
             Aws.Query.Pair ("SourceType", String.to_query f))
       ; Aws.Util.option_map v.subscription_creation_time (fun f ->
             Aws.Query.Pair ("SubscriptionCreationTime", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.sns_topic_arn (fun f ->
             Aws.Query.Pair ("SnsTopicArn", String.to_query f))
       ; Aws.Util.option_map v.cust_subscription_id (fun f ->
             Aws.Query.Pair ("CustSubscriptionId", String.to_query f))
       ; Aws.Util.option_map v.customer_aws_id (fun f ->
             Aws.Query.Pair ("CustomerAwsId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_subscription_arn (fun f ->
             "EventSubscriptionArn", String.to_json f)
       ; Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f)
       ; Some ("EventCategoriesList", EventCategoriesList.to_json v.event_categories_list)
       ; Some ("SourceIdsList", SourceIdsList.to_json v.source_ids_list)
       ; Aws.Util.option_map v.source_type (fun f -> "SourceType", String.to_json f)
       ; Aws.Util.option_map v.subscription_creation_time (fun f ->
             "SubscriptionCreationTime", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.sns_topic_arn (fun f -> "SnsTopicArn", String.to_json f)
       ; Aws.Util.option_map v.cust_subscription_id (fun f ->
             "CustSubscriptionId", String.to_json f)
       ; Aws.Util.option_map v.customer_aws_id (fun f ->
             "CustomerAwsId", String.to_json f)
       ])

let of_json j =
  { customer_aws_id =
      Aws.Util.option_map (Aws.Json.lookup j "CustomerAwsId") String.of_json
  ; cust_subscription_id =
      Aws.Util.option_map (Aws.Json.lookup j "CustSubscriptionId") String.of_json
  ; sns_topic_arn = Aws.Util.option_map (Aws.Json.lookup j "SnsTopicArn") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; subscription_creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "SubscriptionCreationTime") String.of_json
  ; source_type = Aws.Util.option_map (Aws.Json.lookup j "SourceType") String.of_json
  ; source_ids_list =
      SourceIdsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceIdsList"))
  ; event_categories_list =
      EventCategoriesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventCategoriesList"))
  ; enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json
  ; event_subscription_arn =
      Aws.Util.option_map (Aws.Json.lookup j "EventSubscriptionArn") String.of_json
  }
