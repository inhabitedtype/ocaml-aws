open Aws.BaseTypes

type t =
  { subscription_name : String.t
  ; sns_topic_arn : String.t
  ; source_type : String.t option
  ; event_categories : EventCategoriesList.t
  ; source_ids : SourceIdsList.t
  ; enabled : Boolean.t option
  ; tags : TagList.t
  }

let make
    ~subscription_name
    ~sns_topic_arn
    ?source_type
    ?(event_categories = [])
    ?(source_ids = [])
    ?enabled
    ?(tags = [])
    () =
  { subscription_name
  ; sns_topic_arn
  ; source_type
  ; event_categories
  ; source_ids
  ; enabled
  ; tags
  }

let parse xml =
  Some
    { subscription_name =
        Aws.Xml.required
          "SubscriptionName"
          (Aws.Util.option_bind (Aws.Xml.member "SubscriptionName" xml) String.parse)
    ; sns_topic_arn =
        Aws.Xml.required
          "SnsTopicArn"
          (Aws.Util.option_bind (Aws.Xml.member "SnsTopicArn" xml) String.parse)
    ; source_type = Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) String.parse
    ; event_categories =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EventCategories" xml)
             EventCategoriesList.parse)
    ; source_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "SourceIds" xml) SourceIdsList.parse)
    ; enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("SourceIds.member", SourceIdsList.to_query v.source_ids))
       ; Some
           (Aws.Query.Pair
              ("EventCategories.member", EventCategoriesList.to_query v.event_categories))
       ; Aws.Util.option_map v.source_type (fun f ->
             Aws.Query.Pair ("SourceType", String.to_query f))
       ; Some (Aws.Query.Pair ("SnsTopicArn", String.to_query v.sns_topic_arn))
       ; Some (Aws.Query.Pair ("SubscriptionName", String.to_query v.subscription_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f)
       ; Some ("SourceIds", SourceIdsList.to_json v.source_ids)
       ; Some ("EventCategories", EventCategoriesList.to_json v.event_categories)
       ; Aws.Util.option_map v.source_type (fun f -> "SourceType", String.to_json f)
       ; Some ("SnsTopicArn", String.to_json v.sns_topic_arn)
       ; Some ("SubscriptionName", String.to_json v.subscription_name)
       ])

let of_json j =
  { subscription_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubscriptionName"))
  ; sns_topic_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SnsTopicArn"))
  ; source_type = Aws.Util.option_map (Aws.Json.lookup j "SourceType") String.of_json
  ; event_categories =
      EventCategoriesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventCategories"))
  ; source_ids =
      SourceIdsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceIds"))
  ; enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
