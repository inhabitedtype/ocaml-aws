open Aws.BaseTypes

type t =
  { subscription_name : String.t
  ; sns_topic_arn : String.t option
  ; source_type : String.t option
  ; event_categories : EventCategoriesList.t
  ; enabled : Boolean.t option
  }

let make
    ~subscription_name
    ?sns_topic_arn
    ?source_type
    ?(event_categories = [])
    ?enabled
    () =
  { subscription_name; sns_topic_arn; source_type; event_categories; enabled }

let parse xml =
  Some
    { subscription_name =
        Aws.Xml.required
          "SubscriptionName"
          (Aws.Util.option_bind (Aws.Xml.member "SubscriptionName" xml) String.parse)
    ; sns_topic_arn = Aws.Util.option_bind (Aws.Xml.member "SnsTopicArn" xml) String.parse
    ; source_type = Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) String.parse
    ; event_categories =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EventCategories" xml)
             EventCategoriesList.parse)
    ; enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("EventCategories.member", EventCategoriesList.to_query v.event_categories))
       ; Aws.Util.option_map v.source_type (fun f ->
             Aws.Query.Pair ("SourceType", String.to_query f))
       ; Aws.Util.option_map v.sns_topic_arn (fun f ->
             Aws.Query.Pair ("SnsTopicArn", String.to_query f))
       ; Some (Aws.Query.Pair ("SubscriptionName", String.to_query v.subscription_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f)
       ; Some ("EventCategories", EventCategoriesList.to_json v.event_categories)
       ; Aws.Util.option_map v.source_type (fun f -> "SourceType", String.to_json f)
       ; Aws.Util.option_map v.sns_topic_arn (fun f -> "SnsTopicArn", String.to_json f)
       ; Some ("SubscriptionName", String.to_json v.subscription_name)
       ])

let of_json j =
  { subscription_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubscriptionName"))
  ; sns_topic_arn = Aws.Util.option_map (Aws.Json.lookup j "SnsTopicArn") String.of_json
  ; source_type = Aws.Util.option_map (Aws.Json.lookup j "SourceType") String.of_json
  ; event_categories =
      EventCategoriesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventCategories"))
  ; enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json
  }
