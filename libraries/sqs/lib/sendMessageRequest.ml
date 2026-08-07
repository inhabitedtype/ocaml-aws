open Aws.BaseTypes

type t =
  { queue_url : String.t
  ; message_body : String.t
  ; delay_seconds : Integer.t option
  ; message_attributes : MessageBodyAttributeMap.t option
  ; message_system_attributes : MessageBodySystemAttributeMap.t option
  ; message_deduplication_id : String.t option
  ; message_group_id : String.t option
  }

let make
    ~queue_url
    ~message_body
    ?delay_seconds
    ?message_attributes
    ?message_system_attributes
    ?message_deduplication_id
    ?message_group_id
    () =
  { queue_url
  ; message_body
  ; delay_seconds
  ; message_attributes
  ; message_system_attributes
  ; message_deduplication_id
  ; message_group_id
  }

let parse xml =
  Some
    { queue_url =
        Aws.Xml.required
          "QueueUrl"
          (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
    ; message_body =
        Aws.Xml.required
          "MessageBody"
          (Aws.Util.option_bind (Aws.Xml.member "MessageBody" xml) String.parse)
    ; delay_seconds =
        Aws.Util.option_bind (Aws.Xml.member "DelaySeconds" xml) Integer.parse
    ; message_attributes =
        Aws.Util.option_bind
          (Aws.Xml.member "MessageAttributes" xml)
          MessageBodyAttributeMap.parse
    ; message_system_attributes =
        Aws.Util.option_bind
          (Aws.Xml.member "MessageSystemAttributes" xml)
          MessageBodySystemAttributeMap.parse
    ; message_deduplication_id =
        Aws.Util.option_bind (Aws.Xml.member "MessageDeduplicationId" xml) String.parse
    ; message_group_id =
        Aws.Util.option_bind (Aws.Xml.member "MessageGroupId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message_group_id (fun f ->
             Aws.Query.Pair ("MessageGroupId", String.to_query f))
       ; Aws.Util.option_map v.message_deduplication_id (fun f ->
             Aws.Query.Pair ("MessageDeduplicationId", String.to_query f))
       ; Aws.Util.option_map v.message_system_attributes (fun f ->
             Aws.Query.Pair
               ("MessageSystemAttributes", MessageBodySystemAttributeMap.to_query f))
       ; Aws.Util.option_map v.message_attributes (fun f ->
             Aws.Query.Pair ("MessageAttributes", MessageBodyAttributeMap.to_query f))
       ; Aws.Util.option_map v.delay_seconds (fun f ->
             Aws.Query.Pair ("DelaySeconds", Integer.to_query f))
       ; Some (Aws.Query.Pair ("MessageBody", String.to_query v.message_body))
       ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message_group_id (fun f ->
             "MessageGroupId", String.to_json f)
       ; Aws.Util.option_map v.message_deduplication_id (fun f ->
             "MessageDeduplicationId", String.to_json f)
       ; Aws.Util.option_map v.message_system_attributes (fun f ->
             "MessageSystemAttributes", MessageBodySystemAttributeMap.to_json f)
       ; Aws.Util.option_map v.message_attributes (fun f ->
             "MessageAttributes", MessageBodyAttributeMap.to_json f)
       ; Aws.Util.option_map v.delay_seconds (fun f -> "DelaySeconds", Integer.to_json f)
       ; Some ("MessageBody", String.to_json v.message_body)
       ; Some ("QueueUrl", String.to_json v.queue_url)
       ])

let of_json j =
  { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
  ; message_body =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MessageBody"))
  ; delay_seconds = Aws.Util.option_map (Aws.Json.lookup j "DelaySeconds") Integer.of_json
  ; message_attributes =
      Aws.Util.option_map
        (Aws.Json.lookup j "MessageAttributes")
        MessageBodyAttributeMap.of_json
  ; message_system_attributes =
      Aws.Util.option_map
        (Aws.Json.lookup j "MessageSystemAttributes")
        MessageBodySystemAttributeMap.of_json
  ; message_deduplication_id =
      Aws.Util.option_map (Aws.Json.lookup j "MessageDeduplicationId") String.of_json
  ; message_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "MessageGroupId") String.of_json
  }
