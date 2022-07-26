open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

module GetQueueUrlResult = struct
  type t = { queue_url : String.t }

  let make ~queue_url () = { queue_url }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("QueueUrl", String.to_json v.queue_url) ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl")) }
end

module QueueNameExists = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module MessageAttributeNameList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map String.parse (Aws.Xml.members "MessageAttributeName" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ListQueueTagsRequest = struct
  type t = { queue_url : String.t }

  let make ~queue_url () = { queue_url }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("QueueUrl", String.to_json v.queue_url) ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl")) }
end

module PurgeQueueRequest = struct
  type t = { queue_url : String.t }

  let make ~queue_url () = { queue_url }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("QueueUrl", String.to_json v.queue_url) ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl")) }
end

module QueueAttributeName = struct
  type t =
    | All
    | Policy
    | VisibilityTimeout
    | MaximumMessageSize
    | MessageRetentionPeriod
    | ApproximateNumberOfMessages
    | ApproximateNumberOfMessagesNotVisible
    | CreatedTimestamp
    | LastModifiedTimestamp
    | QueueArn
    | ApproximateNumberOfMessagesDelayed
    | DelaySeconds
    | ReceiveMessageWaitTimeSeconds
    | RedrivePolicy
    | FifoQueue
    | ContentBasedDeduplication
    | KmsMasterKeyId
    | KmsDataKeyReusePeriodSeconds

  let str_to_t =
    [ "KmsDataKeyReusePeriodSeconds", KmsDataKeyReusePeriodSeconds
    ; "KmsMasterKeyId", KmsMasterKeyId
    ; "ContentBasedDeduplication", ContentBasedDeduplication
    ; "FifoQueue", FifoQueue
    ; "RedrivePolicy", RedrivePolicy
    ; "ReceiveMessageWaitTimeSeconds", ReceiveMessageWaitTimeSeconds
    ; "DelaySeconds", DelaySeconds
    ; "ApproximateNumberOfMessagesDelayed", ApproximateNumberOfMessagesDelayed
    ; "QueueArn", QueueArn
    ; "LastModifiedTimestamp", LastModifiedTimestamp
    ; "CreatedTimestamp", CreatedTimestamp
    ; "ApproximateNumberOfMessagesNotVisible", ApproximateNumberOfMessagesNotVisible
    ; "ApproximateNumberOfMessages", ApproximateNumberOfMessages
    ; "MessageRetentionPeriod", MessageRetentionPeriod
    ; "MaximumMessageSize", MaximumMessageSize
    ; "VisibilityTimeout", VisibilityTimeout
    ; "Policy", Policy
    ; "All", All
    ]

  let t_to_str =
    [ KmsDataKeyReusePeriodSeconds, "KmsDataKeyReusePeriodSeconds"
    ; KmsMasterKeyId, "KmsMasterKeyId"
    ; ContentBasedDeduplication, "ContentBasedDeduplication"
    ; FifoQueue, "FifoQueue"
    ; RedrivePolicy, "RedrivePolicy"
    ; ReceiveMessageWaitTimeSeconds, "ReceiveMessageWaitTimeSeconds"
    ; DelaySeconds, "DelaySeconds"
    ; ApproximateNumberOfMessagesDelayed, "ApproximateNumberOfMessagesDelayed"
    ; QueueArn, "QueueArn"
    ; LastModifiedTimestamp, "LastModifiedTimestamp"
    ; CreatedTimestamp, "CreatedTimestamp"
    ; ApproximateNumberOfMessagesNotVisible, "ApproximateNumberOfMessagesNotVisible"
    ; ApproximateNumberOfMessages, "ApproximateNumberOfMessages"
    ; MessageRetentionPeriod, "MessageRetentionPeriod"
    ; MaximumMessageSize, "MaximumMessageSize"
    ; VisibilityTimeout, "VisibilityTimeout"
    ; Policy, "Policy"
    ; All, "All"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module QueueAttributeMap = struct
  type t = (QueueAttributeName.t, String.t) Hashtbl.t

  let make elems () = elems

  let parse xml = None

  let to_query v =
    Aws.Query.to_query_hashtbl QueueAttributeName.to_string String.to_query v

  let to_json v =
    `Assoc
      (Hashtbl.fold
         (fun k v acc -> (QueueAttributeName.to_string k, String.to_json v) :: acc)
         v
         [])

  let of_json j = Aws.Json.to_hashtbl QueueAttributeName.of_string String.of_json j
end

module SetQueueAttributesRequest = struct
  type t =
    { queue_url : String.t
    ; attributes : QueueAttributeMap.t
    }

  let make ~queue_url ~attributes () = { queue_url; attributes }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; attributes =
          Aws.Xml.required
            "Attribute"
            (Aws.Util.option_bind
               (Aws.Xml.member "Attribute" xml)
               QueueAttributeMap.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Attribute", QueueAttributeMap.to_query v.attributes))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Attribute", QueueAttributeMap.to_json v.attributes)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; attributes =
        QueueAttributeMap.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Attribute"))
    }
end

module InvalidMessageContents = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module TagMap = struct
  type t = (String.t, String.t) Hashtbl.t

  let make elems () = elems

  let parse xml = None

  let to_query v = Aws.Query.to_query_hashtbl String.to_string String.to_query v

  let to_json v =
    `Assoc
      (Hashtbl.fold (fun k v acc -> (String.to_string k, String.to_json v) :: acc) v [])

  let of_json j = Aws.Json.to_hashtbl String.of_string String.of_json j
end

module CreateQueueRequest = struct
  type t =
    { queue_name : String.t
    ; attributes : QueueAttributeMap.t option
    ; tags : TagMap.t option
    }

  let make ~queue_name ?attributes ?tags () = { queue_name; attributes; tags }

  let parse xml =
    Some
      { queue_name =
          Aws.Xml.required
            "QueueName"
            (Aws.Util.option_bind (Aws.Xml.member "QueueName" xml) String.parse)
      ; attributes =
          Aws.Util.option_bind (Aws.Xml.member "Attribute" xml) QueueAttributeMap.parse
      ; tags = Aws.Util.option_bind (Aws.Xml.member "Tag" xml) TagMap.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.tags (fun f -> Aws.Query.Pair ("Tag", TagMap.to_query f))
         ; Aws.Util.option_map v.attributes (fun f ->
               Aws.Query.Pair ("Attribute", QueueAttributeMap.to_query f))
         ; Some (Aws.Query.Pair ("QueueName", String.to_query v.queue_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.tags (fun f -> "Tag", TagMap.to_json f)
         ; Aws.Util.option_map v.attributes (fun f ->
               "Attribute", QueueAttributeMap.to_json f)
         ; Some ("QueueName", String.to_json v.queue_name)
         ])

  let of_json j =
    { queue_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueName"))
    ; attributes =
        Aws.Util.option_map (Aws.Json.lookup j "Attribute") QueueAttributeMap.of_json
    ; tags = Aws.Util.option_map (Aws.Json.lookup j "Tag") TagMap.of_json
    }
end

module QueueUrlList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "QueueUrl" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ListQueuesResult = struct
  type t =
    { queue_urls : QueueUrlList.t
    ; next_token : String.t option
    }

  let make ?(queue_urls = []) ?next_token () = { queue_urls; next_token }

  let parse xml =
    Some
      { queue_urls = Aws.Util.of_option [] (QueueUrlList.parse xml)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("QueueUrls.member", QueueUrlList.to_query v.queue_urls))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("QueueUrls", QueueUrlList.to_json v.queue_urls)
         ])

  let of_json j =
    { queue_urls =
        QueueUrlList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrls"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DeleteMessageBatchRequestEntry = struct
  type t =
    { id : String.t
    ; receipt_handle : String.t
    }

  let make ~id ~receipt_handle () = { id; receipt_handle }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; receipt_handle =
          Aws.Xml.required
            "ReceiptHandle"
            (Aws.Util.option_bind (Aws.Xml.member "ReceiptHandle" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ReceiptHandle", String.to_query v.receipt_handle))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ReceiptHandle", String.to_json v.receipt_handle)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; receipt_handle =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReceiptHandle"))
    }
end

module ChangeMessageVisibilityBatchResultEntry = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
end

module ChangeMessageVisibilityBatchResultEntryList = struct
  type t = ChangeMessageVisibilityBatchResultEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         ChangeMessageVisibilityBatchResultEntry.parse
         (Aws.Xml.members "ChangeMessageVisibilityBatchResultEntry" xml))

  let to_query v =
    Aws.Query.to_query_list ChangeMessageVisibilityBatchResultEntry.to_query v

  let to_json v = `List (List.map ChangeMessageVisibilityBatchResultEntry.to_json v)

  let of_json j = Aws.Json.to_list ChangeMessageVisibilityBatchResultEntry.of_json j
end

module TooManyEntriesInBatchRequest = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module TagKeyList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "TagKey" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module UntagQueueRequest = struct
  type t =
    { queue_url : String.t
    ; tag_keys : TagKeyList.t
    }

  let make ~queue_url ~tag_keys () = { queue_url; tag_keys }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; tag_keys = Aws.Xml.required "TagKeys" (TagKeyList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TagKeys.member", TagKeyList.to_query v.tag_keys))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TagKeys", TagKeyList.to_json v.tag_keys)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; tag_keys = TagKeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagKeys"))
    }
end

module StringList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "StringListValue" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module BinaryList = struct
  type t = Blob.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Blob.parse (Aws.Xml.members "BinaryListValue" xml))

  let to_query v = Aws.Query.to_query_list Blob.to_query v

  let to_json v = `List (List.map Blob.to_json v)

  let of_json j = Aws.Json.to_list Blob.of_json j
end

module MessageSystemAttributeValue = struct
  type t =
    { string_value : String.t option
    ; binary_value : Blob.t option
    ; string_list_values : StringList.t
    ; binary_list_values : BinaryList.t
    ; data_type : String.t
    }

  let make
      ?string_value
      ?binary_value
      ?(string_list_values = [])
      ?(binary_list_values = [])
      ~data_type
      () =
    { string_value; binary_value; string_list_values; binary_list_values; data_type }

  let parse xml =
    Some
      { string_value =
          Aws.Util.option_bind (Aws.Xml.member "StringValue" xml) String.parse
      ; binary_value = Aws.Util.option_bind (Aws.Xml.member "BinaryValue" xml) Blob.parse
      ; string_list_values =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "StringListValue" xml) StringList.parse)
      ; binary_list_values =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "BinaryListValue" xml) BinaryList.parse)
      ; data_type =
          Aws.Xml.required
            "DataType"
            (Aws.Util.option_bind (Aws.Xml.member "DataType" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("DataType", String.to_query v.data_type))
         ; Some
             (Aws.Query.Pair ("BinaryListValue", BinaryList.to_query v.binary_list_values))
         ; Some
             (Aws.Query.Pair ("StringListValue", StringList.to_query v.string_list_values))
         ; Aws.Util.option_map v.binary_value (fun f ->
               Aws.Query.Pair ("BinaryValue", Blob.to_query f))
         ; Aws.Util.option_map v.string_value (fun f ->
               Aws.Query.Pair ("StringValue", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("DataType", String.to_json v.data_type)
         ; Some ("BinaryListValue", BinaryList.to_json v.binary_list_values)
         ; Some ("StringListValue", StringList.to_json v.string_list_values)
         ; Aws.Util.option_map v.binary_value (fun f -> "BinaryValue", Blob.to_json f)
         ; Aws.Util.option_map v.string_value (fun f -> "StringValue", String.to_json f)
         ])

  let of_json j =
    { string_value = Aws.Util.option_map (Aws.Json.lookup j "StringValue") String.of_json
    ; binary_value = Aws.Util.option_map (Aws.Json.lookup j "BinaryValue") Blob.of_json
    ; string_list_values =
        StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StringListValue"))
    ; binary_list_values =
        BinaryList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "BinaryListValue"))
    ; data_type = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DataType"))
    }
end

module ListQueueTagsResult = struct
  type t = { tags : TagMap.t option }

  let make ?tags () = { tags }

  let parse xml =
    Some { tags = Aws.Util.option_bind (Aws.Xml.member "Tag" xml) TagMap.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.tags (fun f -> Aws.Query.Pair ("Tag", TagMap.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.tags (fun f -> "Tag", TagMap.to_json f) ])

  let of_json j = { tags = Aws.Util.option_map (Aws.Json.lookup j "Tag") TagMap.of_json }
end

module MessageSystemAttributeName = struct
  type t =
    | SenderId
    | SentTimestamp
    | ApproximateReceiveCount
    | ApproximateFirstReceiveTimestamp
    | SequenceNumber
    | MessageDeduplicationId
    | MessageGroupId
    | AWSTraceHeader

  let str_to_t =
    [ "AWSTraceHeader", AWSTraceHeader
    ; "MessageGroupId", MessageGroupId
    ; "MessageDeduplicationId", MessageDeduplicationId
    ; "SequenceNumber", SequenceNumber
    ; "ApproximateFirstReceiveTimestamp", ApproximateFirstReceiveTimestamp
    ; "ApproximateReceiveCount", ApproximateReceiveCount
    ; "SentTimestamp", SentTimestamp
    ; "SenderId", SenderId
    ]

  let t_to_str =
    [ AWSTraceHeader, "AWSTraceHeader"
    ; MessageGroupId, "MessageGroupId"
    ; MessageDeduplicationId, "MessageDeduplicationId"
    ; SequenceNumber, "SequenceNumber"
    ; ApproximateFirstReceiveTimestamp, "ApproximateFirstReceiveTimestamp"
    ; ApproximateReceiveCount, "ApproximateReceiveCount"
    ; SentTimestamp, "SentTimestamp"
    ; SenderId, "SenderId"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module MessageSystemAttributeMap = struct
  type t = (MessageSystemAttributeName.t, String.t) Hashtbl.t

  let make elems () = elems

  let parse xml = None

  let to_query v =
    Aws.Query.to_query_hashtbl MessageSystemAttributeName.to_string String.to_query v

  let to_json v =
    `Assoc
      (Hashtbl.fold
         (fun k v acc ->
           (MessageSystemAttributeName.to_string k, String.to_json v) :: acc)
         v
         [])

  let of_json j =
    Aws.Json.to_hashtbl MessageSystemAttributeName.of_string String.of_json j
end

module MessageAttributeValue = struct
  type t =
    { string_value : String.t option
    ; binary_value : Blob.t option
    ; string_list_values : StringList.t
    ; binary_list_values : BinaryList.t
    ; data_type : String.t
    }

  let make
      ?string_value
      ?binary_value
      ?(string_list_values = [])
      ?(binary_list_values = [])
      ~data_type
      () =
    { string_value; binary_value; string_list_values; binary_list_values; data_type }

  let parse xml =
    Some
      { string_value =
          Aws.Util.option_bind (Aws.Xml.member "StringValue" xml) String.parse
      ; binary_value = Aws.Util.option_bind (Aws.Xml.member "BinaryValue" xml) Blob.parse
      ; string_list_values =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "StringListValue" xml) StringList.parse)
      ; binary_list_values =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "BinaryListValue" xml) BinaryList.parse)
      ; data_type =
          Aws.Xml.required
            "DataType"
            (Aws.Util.option_bind (Aws.Xml.member "DataType" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("DataType", String.to_query v.data_type))
         ; Some
             (Aws.Query.Pair ("BinaryListValue", BinaryList.to_query v.binary_list_values))
         ; Some
             (Aws.Query.Pair ("StringListValue", StringList.to_query v.string_list_values))
         ; Aws.Util.option_map v.binary_value (fun f ->
               Aws.Query.Pair ("BinaryValue", Blob.to_query f))
         ; Aws.Util.option_map v.string_value (fun f ->
               Aws.Query.Pair ("StringValue", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("DataType", String.to_json v.data_type)
         ; Some ("BinaryListValue", BinaryList.to_json v.binary_list_values)
         ; Some ("StringListValue", StringList.to_json v.string_list_values)
         ; Aws.Util.option_map v.binary_value (fun f -> "BinaryValue", Blob.to_json f)
         ; Aws.Util.option_map v.string_value (fun f -> "StringValue", String.to_json f)
         ])

  let of_json j =
    { string_value = Aws.Util.option_map (Aws.Json.lookup j "StringValue") String.of_json
    ; binary_value = Aws.Util.option_map (Aws.Json.lookup j "BinaryValue") Blob.of_json
    ; string_list_values =
        StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StringListValue"))
    ; binary_list_values =
        BinaryList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "BinaryListValue"))
    ; data_type = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DataType"))
    }
end

module MessageBodyAttributeMap = struct
  type t = (String.t, MessageAttributeValue.t) Hashtbl.t

  let make elems () = elems

  let parse xml = None

  let to_query v =
    Aws.Query.to_query_hashtbl String.to_string MessageAttributeValue.to_query v

  let to_json v =
    `Assoc
      (Hashtbl.fold
         (fun k v acc -> (String.to_string k, MessageAttributeValue.to_json v) :: acc)
         v
         [])

  let of_json j = Aws.Json.to_hashtbl String.of_string MessageAttributeValue.of_json j
end

module Message = struct
  type t =
    { message_id : String.t option
    ; receipt_handle : String.t option
    ; m_d5_of_body : String.t option
    ; body : String.t option
    ; attributes : MessageSystemAttributeMap.t option
    ; m_d5_of_message_attributes : String.t option
    ; message_attributes : MessageBodyAttributeMap.t option
    }

  let make
      ?message_id
      ?receipt_handle
      ?m_d5_of_body
      ?body
      ?attributes
      ?m_d5_of_message_attributes
      ?message_attributes
      () =
    { message_id
    ; receipt_handle
    ; m_d5_of_body
    ; body
    ; attributes
    ; m_d5_of_message_attributes
    ; message_attributes
    }

  let parse xml =
    Some
      { message_id = Aws.Util.option_bind (Aws.Xml.member "MessageId" xml) String.parse
      ; receipt_handle =
          Aws.Util.option_bind (Aws.Xml.member "ReceiptHandle" xml) String.parse
      ; m_d5_of_body = Aws.Util.option_bind (Aws.Xml.member "MD5OfBody" xml) String.parse
      ; body = Aws.Util.option_bind (Aws.Xml.member "Body" xml) String.parse
      ; attributes =
          Aws.Util.option_bind
            (Aws.Xml.member "Attribute" xml)
            MessageSystemAttributeMap.parse
      ; m_d5_of_message_attributes =
          Aws.Util.option_bind (Aws.Xml.member "MD5OfMessageAttributes" xml) String.parse
      ; message_attributes =
          Aws.Util.option_bind
            (Aws.Xml.member "MessageAttribute" xml)
            MessageBodyAttributeMap.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message_attributes (fun f ->
               Aws.Query.Pair ("MessageAttribute", MessageBodyAttributeMap.to_query f))
         ; Aws.Util.option_map v.m_d5_of_message_attributes (fun f ->
               Aws.Query.Pair ("MD5OfMessageAttributes", String.to_query f))
         ; Aws.Util.option_map v.attributes (fun f ->
               Aws.Query.Pair ("Attribute", MessageSystemAttributeMap.to_query f))
         ; Aws.Util.option_map v.body (fun f ->
               Aws.Query.Pair ("Body", String.to_query f))
         ; Aws.Util.option_map v.m_d5_of_body (fun f ->
               Aws.Query.Pair ("MD5OfBody", String.to_query f))
         ; Aws.Util.option_map v.receipt_handle (fun f ->
               Aws.Query.Pair ("ReceiptHandle", String.to_query f))
         ; Aws.Util.option_map v.message_id (fun f ->
               Aws.Query.Pair ("MessageId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message_attributes (fun f ->
               "MessageAttribute", MessageBodyAttributeMap.to_json f)
         ; Aws.Util.option_map v.m_d5_of_message_attributes (fun f ->
               "MD5OfMessageAttributes", String.to_json f)
         ; Aws.Util.option_map v.attributes (fun f ->
               "Attribute", MessageSystemAttributeMap.to_json f)
         ; Aws.Util.option_map v.body (fun f -> "Body", String.to_json f)
         ; Aws.Util.option_map v.m_d5_of_body (fun f -> "MD5OfBody", String.to_json f)
         ; Aws.Util.option_map v.receipt_handle (fun f ->
               "ReceiptHandle", String.to_json f)
         ; Aws.Util.option_map v.message_id (fun f -> "MessageId", String.to_json f)
         ])

  let of_json j =
    { message_id = Aws.Util.option_map (Aws.Json.lookup j "MessageId") String.of_json
    ; receipt_handle =
        Aws.Util.option_map (Aws.Json.lookup j "ReceiptHandle") String.of_json
    ; m_d5_of_body = Aws.Util.option_map (Aws.Json.lookup j "MD5OfBody") String.of_json
    ; body = Aws.Util.option_map (Aws.Json.lookup j "Body") String.of_json
    ; attributes =
        Aws.Util.option_map
          (Aws.Json.lookup j "Attribute")
          MessageSystemAttributeMap.of_json
    ; m_d5_of_message_attributes =
        Aws.Util.option_map (Aws.Json.lookup j "MD5OfMessageAttributes") String.of_json
    ; message_attributes =
        Aws.Util.option_map
          (Aws.Json.lookup j "MessageAttribute")
          MessageBodyAttributeMap.of_json
    }
end

module DeleteMessageBatchResultEntry = struct
  type t = { id : String.t }

  let make ~id () = { id }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Id", String.to_query v.id)) ])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.id) ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
end

module DeleteMessageBatchResultEntryList = struct
  type t = DeleteMessageBatchResultEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         DeleteMessageBatchResultEntry.parse
         (Aws.Xml.members "DeleteMessageBatchResultEntry" xml))

  let to_query v = Aws.Query.to_query_list DeleteMessageBatchResultEntry.to_query v

  let to_json v = `List (List.map DeleteMessageBatchResultEntry.to_json v)

  let of_json j = Aws.Json.to_list DeleteMessageBatchResultEntry.of_json j
end

module BatchResultErrorEntry = struct
  type t =
    { id : String.t
    ; sender_fault : Boolean.t
    ; code : String.t
    ; message : String.t option
    }

  let make ~id ~sender_fault ~code ?message () = { id; sender_fault; code; message }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; sender_fault =
          Aws.Xml.required
            "SenderFault"
            (Aws.Util.option_bind (Aws.Xml.member "SenderFault" xml) Boolean.parse)
      ; code =
          Aws.Xml.required
            "Code"
            (Aws.Util.option_bind (Aws.Xml.member "Code" xml) String.parse)
      ; message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("Message", String.to_query f))
         ; Some (Aws.Query.Pair ("Code", String.to_query v.code))
         ; Some (Aws.Query.Pair ("SenderFault", Boolean.to_query v.sender_fault))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "Message", String.to_json f)
         ; Some ("Code", String.to_json v.code)
         ; Some ("SenderFault", Boolean.to_json v.sender_fault)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; sender_fault =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SenderFault"))
    ; code = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Code"))
    ; message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json
    }
end

module BatchResultErrorEntryList = struct
  type t = BatchResultErrorEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map BatchResultErrorEntry.parse (Aws.Xml.members "BatchResultErrorEntry" xml))

  let to_query v = Aws.Query.to_query_list BatchResultErrorEntry.to_query v

  let to_json v = `List (List.map BatchResultErrorEntry.to_json v)

  let of_json j = Aws.Json.to_list BatchResultErrorEntry.of_json j
end

module DeleteMessageBatchResult = struct
  type t =
    { successful : DeleteMessageBatchResultEntryList.t
    ; failed : BatchResultErrorEntryList.t
    }

  let make ~successful ~failed () = { successful; failed }

  let parse xml =
    Some
      { successful =
          Aws.Xml.required "Successful" (DeleteMessageBatchResultEntryList.parse xml)
      ; failed = Aws.Xml.required "Failed" (BatchResultErrorEntryList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("Failed.member", BatchResultErrorEntryList.to_query v.failed))
         ; Some
             (Aws.Query.Pair
                ( "Successful.member"
                , DeleteMessageBatchResultEntryList.to_query v.successful ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Failed", BatchResultErrorEntryList.to_json v.failed)
         ; Some ("Successful", DeleteMessageBatchResultEntryList.to_json v.successful)
         ])

  let of_json j =
    { successful =
        DeleteMessageBatchResultEntryList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Successful"))
    ; failed =
        BatchResultErrorEntryList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Failed"))
    }
end

module InvalidBatchEntryId = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module AttributeNameList = struct
  type t = QueueAttributeName.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map QueueAttributeName.parse (Aws.Xml.members "AttributeName" xml))

  let to_query v = Aws.Query.to_query_list QueueAttributeName.to_query v

  let to_json v = `List (List.map QueueAttributeName.to_json v)

  let of_json j = Aws.Json.to_list QueueAttributeName.of_json j
end

module GetQueueAttributesRequest = struct
  type t =
    { queue_url : String.t
    ; attribute_names : AttributeNameList.t
    }

  let make ~queue_url ?(attribute_names = []) () = { queue_url; attribute_names }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; attribute_names = Aws.Util.of_option [] (AttributeNameList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("AttributeNames.member", AttributeNameList.to_query v.attribute_names))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AttributeNames", AttributeNameList.to_json v.attribute_names)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; attribute_names =
        AttributeNameList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeNames"))
    }
end

module MessageList = struct
  type t = Message.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Message.parse (Aws.Xml.members "Message" xml))

  let to_query v = Aws.Query.to_query_list Message.to_query v

  let to_json v = `List (List.map Message.to_json v)

  let of_json j = Aws.Json.to_list Message.of_json j
end

module ReceiveMessageResult = struct
  type t = { messages : MessageList.t }

  let make ?(messages = []) () = { messages }

  let parse xml = Some { messages = Aws.Util.of_option [] (MessageList.parse xml) }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Messages.member", MessageList.to_query v.messages)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("Messages", MessageList.to_json v.messages) ])

  let of_json j =
    { messages =
        MessageList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Messages"))
    }
end

module OverLimit = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteMessageBatchRequestEntryList = struct
  type t = DeleteMessageBatchRequestEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         DeleteMessageBatchRequestEntry.parse
         (Aws.Xml.members "DeleteMessageBatchRequestEntry" xml))

  let to_query v = Aws.Query.to_query_list DeleteMessageBatchRequestEntry.to_query v

  let to_json v = `List (List.map DeleteMessageBatchRequestEntry.to_json v)

  let of_json j = Aws.Json.to_list DeleteMessageBatchRequestEntry.of_json j
end

module MessageSystemAttributeNameForSends = struct
  type t = AWSTraceHeader

  let str_to_t = [ "AWSTraceHeader", AWSTraceHeader ]

  let t_to_str = [ AWSTraceHeader, "AWSTraceHeader" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module MessageBodySystemAttributeMap = struct
  type t = (MessageSystemAttributeNameForSends.t, MessageSystemAttributeValue.t) Hashtbl.t

  let make elems () = elems

  let parse xml = None

  let to_query v =
    Aws.Query.to_query_hashtbl
      MessageSystemAttributeNameForSends.to_string
      MessageSystemAttributeValue.to_query
      v

  let to_json v =
    `Assoc
      (Hashtbl.fold
         (fun k v acc ->
           ( MessageSystemAttributeNameForSends.to_string k
           , MessageSystemAttributeValue.to_json v )
           :: acc)
         v
         [])

  let of_json j =
    Aws.Json.to_hashtbl
      MessageSystemAttributeNameForSends.of_string
      MessageSystemAttributeValue.of_json
      j
end

module SendMessageBatchRequestEntry = struct
  type t =
    { id : String.t
    ; message_body : String.t
    ; delay_seconds : Integer.t option
    ; message_attributes : MessageBodyAttributeMap.t option
    ; message_system_attributes : MessageBodySystemAttributeMap.t option
    ; message_deduplication_id : String.t option
    ; message_group_id : String.t option
    }

  let make
      ~id
      ~message_body
      ?delay_seconds
      ?message_attributes
      ?message_system_attributes
      ?message_deduplication_id
      ?message_group_id
      () =
    { id
    ; message_body
    ; delay_seconds
    ; message_attributes
    ; message_system_attributes
    ; message_deduplication_id
    ; message_group_id
    }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; message_body =
          Aws.Xml.required
            "MessageBody"
            (Aws.Util.option_bind (Aws.Xml.member "MessageBody" xml) String.parse)
      ; delay_seconds =
          Aws.Util.option_bind (Aws.Xml.member "DelaySeconds" xml) Integer.parse
      ; message_attributes =
          Aws.Util.option_bind
            (Aws.Xml.member "MessageAttribute" xml)
            MessageBodyAttributeMap.parse
      ; message_system_attributes =
          Aws.Util.option_bind
            (Aws.Xml.member "MessageSystemAttribute" xml)
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
                 ("MessageSystemAttribute", MessageBodySystemAttributeMap.to_query f))
         ; Aws.Util.option_map v.message_attributes (fun f ->
               Aws.Query.Pair ("MessageAttribute", MessageBodyAttributeMap.to_query f))
         ; Aws.Util.option_map v.delay_seconds (fun f ->
               Aws.Query.Pair ("DelaySeconds", Integer.to_query f))
         ; Some (Aws.Query.Pair ("MessageBody", String.to_query v.message_body))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message_group_id (fun f ->
               "MessageGroupId", String.to_json f)
         ; Aws.Util.option_map v.message_deduplication_id (fun f ->
               "MessageDeduplicationId", String.to_json f)
         ; Aws.Util.option_map v.message_system_attributes (fun f ->
               "MessageSystemAttribute", MessageBodySystemAttributeMap.to_json f)
         ; Aws.Util.option_map v.message_attributes (fun f ->
               "MessageAttribute", MessageBodyAttributeMap.to_json f)
         ; Aws.Util.option_map v.delay_seconds (fun f ->
               "DelaySeconds", Integer.to_json f)
         ; Some ("MessageBody", String.to_json v.message_body)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; message_body =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MessageBody"))
    ; delay_seconds =
        Aws.Util.option_map (Aws.Json.lookup j "DelaySeconds") Integer.of_json
    ; message_attributes =
        Aws.Util.option_map
          (Aws.Json.lookup j "MessageAttribute")
          MessageBodyAttributeMap.of_json
    ; message_system_attributes =
        Aws.Util.option_map
          (Aws.Json.lookup j "MessageSystemAttribute")
          MessageBodySystemAttributeMap.of_json
    ; message_deduplication_id =
        Aws.Util.option_map (Aws.Json.lookup j "MessageDeduplicationId") String.of_json
    ; message_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "MessageGroupId") String.of_json
    }
end

module SendMessageBatchRequestEntryList = struct
  type t = SendMessageBatchRequestEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         SendMessageBatchRequestEntry.parse
         (Aws.Xml.members "SendMessageBatchRequestEntry" xml))

  let to_query v = Aws.Query.to_query_list SendMessageBatchRequestEntry.to_query v

  let to_json v = `List (List.map SendMessageBatchRequestEntry.to_json v)

  let of_json j = Aws.Json.to_list SendMessageBatchRequestEntry.of_json j
end

module InvalidAttributeName = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module TagQueueRequest = struct
  type t =
    { queue_url : String.t
    ; tags : TagMap.t
    }

  let make ~queue_url ~tags () = { queue_url; tags }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; tags =
          Aws.Xml.required
            "Tags"
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagMap.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags", TagMap.to_query v.tags))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagMap.to_json v.tags)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; tags = TagMap.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module SendMessageBatchResultEntry = struct
  type t =
    { id : String.t
    ; message_id : String.t
    ; m_d5_of_message_body : String.t
    ; m_d5_of_message_attributes : String.t option
    ; m_d5_of_message_system_attributes : String.t option
    ; sequence_number : String.t option
    }

  let make
      ~id
      ~message_id
      ~m_d5_of_message_body
      ?m_d5_of_message_attributes
      ?m_d5_of_message_system_attributes
      ?sequence_number
      () =
    { id
    ; message_id
    ; m_d5_of_message_body
    ; m_d5_of_message_attributes
    ; m_d5_of_message_system_attributes
    ; sequence_number
    }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; message_id =
          Aws.Xml.required
            "MessageId"
            (Aws.Util.option_bind (Aws.Xml.member "MessageId" xml) String.parse)
      ; m_d5_of_message_body =
          Aws.Xml.required
            "MD5OfMessageBody"
            (Aws.Util.option_bind (Aws.Xml.member "MD5OfMessageBody" xml) String.parse)
      ; m_d5_of_message_attributes =
          Aws.Util.option_bind (Aws.Xml.member "MD5OfMessageAttributes" xml) String.parse
      ; m_d5_of_message_system_attributes =
          Aws.Util.option_bind
            (Aws.Xml.member "MD5OfMessageSystemAttributes" xml)
            String.parse
      ; sequence_number =
          Aws.Util.option_bind (Aws.Xml.member "SequenceNumber" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.sequence_number (fun f ->
               Aws.Query.Pair ("SequenceNumber", String.to_query f))
         ; Aws.Util.option_map v.m_d5_of_message_system_attributes (fun f ->
               Aws.Query.Pair ("MD5OfMessageSystemAttributes", String.to_query f))
         ; Aws.Util.option_map v.m_d5_of_message_attributes (fun f ->
               Aws.Query.Pair ("MD5OfMessageAttributes", String.to_query f))
         ; Some
             (Aws.Query.Pair ("MD5OfMessageBody", String.to_query v.m_d5_of_message_body))
         ; Some (Aws.Query.Pair ("MessageId", String.to_query v.message_id))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.sequence_number (fun f ->
               "SequenceNumber", String.to_json f)
         ; Aws.Util.option_map v.m_d5_of_message_system_attributes (fun f ->
               "MD5OfMessageSystemAttributes", String.to_json f)
         ; Aws.Util.option_map v.m_d5_of_message_attributes (fun f ->
               "MD5OfMessageAttributes", String.to_json f)
         ; Some ("MD5OfMessageBody", String.to_json v.m_d5_of_message_body)
         ; Some ("MessageId", String.to_json v.message_id)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; message_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MessageId"))
    ; m_d5_of_message_body =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MD5OfMessageBody"))
    ; m_d5_of_message_attributes =
        Aws.Util.option_map (Aws.Json.lookup j "MD5OfMessageAttributes") String.of_json
    ; m_d5_of_message_system_attributes =
        Aws.Util.option_map
          (Aws.Json.lookup j "MD5OfMessageSystemAttributes")
          String.of_json
    ; sequence_number =
        Aws.Util.option_map (Aws.Json.lookup j "SequenceNumber") String.of_json
    }
end

module CreateQueueResult = struct
  type t = { queue_url : String.t option }

  let make ?queue_url () = { queue_url }

  let parse xml =
    Some { queue_url = Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.queue_url (fun f ->
               Aws.Query.Pair ("QueueUrl", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.queue_url (fun f -> "QueueUrl", String.to_json f) ])

  let of_json j =
    { queue_url = Aws.Util.option_map (Aws.Json.lookup j "QueueUrl") String.of_json }
end

module BatchEntryIdsNotDistinct = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module UnsupportedOperation = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListQueuesRequest = struct
  type t =
    { queue_name_prefix : String.t option
    ; next_token : String.t option
    ; max_results : Integer.t option
    }

  let make ?queue_name_prefix ?next_token ?max_results () =
    { queue_name_prefix; next_token; max_results }

  let parse xml =
    Some
      { queue_name_prefix =
          Aws.Util.option_bind (Aws.Xml.member "QueueNamePrefix" xml) String.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.queue_name_prefix (fun f ->
               Aws.Query.Pair ("QueueNamePrefix", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.queue_name_prefix (fun f ->
               "QueueNamePrefix", String.to_json f)
         ])

  let of_json j =
    { queue_name_prefix =
        Aws.Util.option_map (Aws.Json.lookup j "QueueNamePrefix") String.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    }
end

module ChangeMessageVisibilityBatchRequestEntry = struct
  type t =
    { id : String.t
    ; receipt_handle : String.t
    ; visibility_timeout : Integer.t option
    }

  let make ~id ~receipt_handle ?visibility_timeout () =
    { id; receipt_handle; visibility_timeout }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; receipt_handle =
          Aws.Xml.required
            "ReceiptHandle"
            (Aws.Util.option_bind (Aws.Xml.member "ReceiptHandle" xml) String.parse)
      ; visibility_timeout =
          Aws.Util.option_bind (Aws.Xml.member "VisibilityTimeout" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.visibility_timeout (fun f ->
               Aws.Query.Pair ("VisibilityTimeout", Integer.to_query f))
         ; Some (Aws.Query.Pair ("ReceiptHandle", String.to_query v.receipt_handle))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.visibility_timeout (fun f ->
               "VisibilityTimeout", Integer.to_json f)
         ; Some ("ReceiptHandle", String.to_json v.receipt_handle)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; receipt_handle =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReceiptHandle"))
    ; visibility_timeout =
        Aws.Util.option_map (Aws.Json.lookup j "VisibilityTimeout") Integer.of_json
    }
end

module QueueDoesNotExist = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListDeadLetterSourceQueuesResult = struct
  type t =
    { queue_urls : QueueUrlList.t
    ; next_token : String.t option
    }

  let make ~queue_urls ?next_token () = { queue_urls; next_token }

  let parse xml =
    Some
      { queue_urls = Aws.Xml.required "queueUrls" (QueueUrlList.parse xml)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("queueUrls.member", QueueUrlList.to_query v.queue_urls))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("queueUrls", QueueUrlList.to_json v.queue_urls)
         ])

  let of_json j =
    { queue_urls =
        QueueUrlList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "queueUrls"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module BatchRequestTooLong = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module SendMessageRequest = struct
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
            (Aws.Xml.member "MessageAttribute" xml)
            MessageBodyAttributeMap.parse
      ; message_system_attributes =
          Aws.Util.option_bind
            (Aws.Xml.member "MessageSystemAttribute" xml)
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
                 ("MessageSystemAttribute", MessageBodySystemAttributeMap.to_query f))
         ; Aws.Util.option_map v.message_attributes (fun f ->
               Aws.Query.Pair ("MessageAttribute", MessageBodyAttributeMap.to_query f))
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
               "MessageSystemAttribute", MessageBodySystemAttributeMap.to_json f)
         ; Aws.Util.option_map v.message_attributes (fun f ->
               "MessageAttribute", MessageBodyAttributeMap.to_json f)
         ; Aws.Util.option_map v.delay_seconds (fun f ->
               "DelaySeconds", Integer.to_json f)
         ; Some ("MessageBody", String.to_json v.message_body)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; message_body =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MessageBody"))
    ; delay_seconds =
        Aws.Util.option_map (Aws.Json.lookup j "DelaySeconds") Integer.of_json
    ; message_attributes =
        Aws.Util.option_map
          (Aws.Json.lookup j "MessageAttribute")
          MessageBodyAttributeMap.of_json
    ; message_system_attributes =
        Aws.Util.option_map
          (Aws.Json.lookup j "MessageSystemAttribute")
          MessageBodySystemAttributeMap.of_json
    ; message_deduplication_id =
        Aws.Util.option_map (Aws.Json.lookup j "MessageDeduplicationId") String.of_json
    ; message_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "MessageGroupId") String.of_json
    }
end

module AWSAccountIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "AWSAccountId" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module QueueDeletedRecently = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListDeadLetterSourceQueuesRequest = struct
  type t =
    { queue_url : String.t
    ; next_token : String.t option
    ; max_results : Integer.t option
    }

  let make ~queue_url ?next_token ?max_results () = { queue_url; next_token; max_results }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    }
end

module DeleteMessageRequest = struct
  type t =
    { queue_url : String.t
    ; receipt_handle : String.t
    }

  let make ~queue_url ~receipt_handle () = { queue_url; receipt_handle }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; receipt_handle =
          Aws.Xml.required
            "ReceiptHandle"
            (Aws.Util.option_bind (Aws.Xml.member "ReceiptHandle" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ReceiptHandle", String.to_query v.receipt_handle))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ReceiptHandle", String.to_json v.receipt_handle)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; receipt_handle =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReceiptHandle"))
    }
end

module SendMessageResult = struct
  type t =
    { m_d5_of_message_body : String.t option
    ; m_d5_of_message_attributes : String.t option
    ; m_d5_of_message_system_attributes : String.t option
    ; message_id : String.t option
    ; sequence_number : String.t option
    }

  let make
      ?m_d5_of_message_body
      ?m_d5_of_message_attributes
      ?m_d5_of_message_system_attributes
      ?message_id
      ?sequence_number
      () =
    { m_d5_of_message_body
    ; m_d5_of_message_attributes
    ; m_d5_of_message_system_attributes
    ; message_id
    ; sequence_number
    }

  let parse xml =
    Some
      { m_d5_of_message_body =
          Aws.Util.option_bind (Aws.Xml.member "MD5OfMessageBody" xml) String.parse
      ; m_d5_of_message_attributes =
          Aws.Util.option_bind (Aws.Xml.member "MD5OfMessageAttributes" xml) String.parse
      ; m_d5_of_message_system_attributes =
          Aws.Util.option_bind
            (Aws.Xml.member "MD5OfMessageSystemAttributes" xml)
            String.parse
      ; message_id = Aws.Util.option_bind (Aws.Xml.member "MessageId" xml) String.parse
      ; sequence_number =
          Aws.Util.option_bind (Aws.Xml.member "SequenceNumber" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.sequence_number (fun f ->
               Aws.Query.Pair ("SequenceNumber", String.to_query f))
         ; Aws.Util.option_map v.message_id (fun f ->
               Aws.Query.Pair ("MessageId", String.to_query f))
         ; Aws.Util.option_map v.m_d5_of_message_system_attributes (fun f ->
               Aws.Query.Pair ("MD5OfMessageSystemAttributes", String.to_query f))
         ; Aws.Util.option_map v.m_d5_of_message_attributes (fun f ->
               Aws.Query.Pair ("MD5OfMessageAttributes", String.to_query f))
         ; Aws.Util.option_map v.m_d5_of_message_body (fun f ->
               Aws.Query.Pair ("MD5OfMessageBody", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.sequence_number (fun f ->
               "SequenceNumber", String.to_json f)
         ; Aws.Util.option_map v.message_id (fun f -> "MessageId", String.to_json f)
         ; Aws.Util.option_map v.m_d5_of_message_system_attributes (fun f ->
               "MD5OfMessageSystemAttributes", String.to_json f)
         ; Aws.Util.option_map v.m_d5_of_message_attributes (fun f ->
               "MD5OfMessageAttributes", String.to_json f)
         ; Aws.Util.option_map v.m_d5_of_message_body (fun f ->
               "MD5OfMessageBody", String.to_json f)
         ])

  let of_json j =
    { m_d5_of_message_body =
        Aws.Util.option_map (Aws.Json.lookup j "MD5OfMessageBody") String.of_json
    ; m_d5_of_message_attributes =
        Aws.Util.option_map (Aws.Json.lookup j "MD5OfMessageAttributes") String.of_json
    ; m_d5_of_message_system_attributes =
        Aws.Util.option_map
          (Aws.Json.lookup j "MD5OfMessageSystemAttributes")
          String.of_json
    ; message_id = Aws.Util.option_map (Aws.Json.lookup j "MessageId") String.of_json
    ; sequence_number =
        Aws.Util.option_map (Aws.Json.lookup j "SequenceNumber") String.of_json
    }
end

module ReceiveMessageRequest = struct
  type t =
    { queue_url : String.t
    ; attribute_names : AttributeNameList.t
    ; message_attribute_names : MessageAttributeNameList.t
    ; max_number_of_messages : Integer.t option
    ; visibility_timeout : Integer.t option
    ; wait_time_seconds : Integer.t option
    ; receive_request_attempt_id : String.t option
    }

  let make
      ~queue_url
      ?(attribute_names = [])
      ?(message_attribute_names = [])
      ?max_number_of_messages
      ?visibility_timeout
      ?wait_time_seconds
      ?receive_request_attempt_id
      () =
    { queue_url
    ; attribute_names
    ; message_attribute_names
    ; max_number_of_messages
    ; visibility_timeout
    ; wait_time_seconds
    ; receive_request_attempt_id
    }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; attribute_names = Aws.Util.of_option [] (AttributeNameList.parse xml)
      ; message_attribute_names =
          Aws.Util.of_option [] (MessageAttributeNameList.parse xml)
      ; max_number_of_messages =
          Aws.Util.option_bind (Aws.Xml.member "MaxNumberOfMessages" xml) Integer.parse
      ; visibility_timeout =
          Aws.Util.option_bind (Aws.Xml.member "VisibilityTimeout" xml) Integer.parse
      ; wait_time_seconds =
          Aws.Util.option_bind (Aws.Xml.member "WaitTimeSeconds" xml) Integer.parse
      ; receive_request_attempt_id =
          Aws.Util.option_bind (Aws.Xml.member "ReceiveRequestAttemptId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.receive_request_attempt_id (fun f ->
               Aws.Query.Pair ("ReceiveRequestAttemptId", String.to_query f))
         ; Aws.Util.option_map v.wait_time_seconds (fun f ->
               Aws.Query.Pair ("WaitTimeSeconds", Integer.to_query f))
         ; Aws.Util.option_map v.visibility_timeout (fun f ->
               Aws.Query.Pair ("VisibilityTimeout", Integer.to_query f))
         ; Aws.Util.option_map v.max_number_of_messages (fun f ->
               Aws.Query.Pair ("MaxNumberOfMessages", Integer.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "MessageAttributeNames.member"
                , MessageAttributeNameList.to_query v.message_attribute_names ))
         ; Some
             (Aws.Query.Pair
                ("AttributeNames.member", AttributeNameList.to_query v.attribute_names))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.receive_request_attempt_id (fun f ->
               "ReceiveRequestAttemptId", String.to_json f)
         ; Aws.Util.option_map v.wait_time_seconds (fun f ->
               "WaitTimeSeconds", Integer.to_json f)
         ; Aws.Util.option_map v.visibility_timeout (fun f ->
               "VisibilityTimeout", Integer.to_json f)
         ; Aws.Util.option_map v.max_number_of_messages (fun f ->
               "MaxNumberOfMessages", Integer.to_json f)
         ; Some
             ( "MessageAttributeNames"
             , MessageAttributeNameList.to_json v.message_attribute_names )
         ; Some ("AttributeNames", AttributeNameList.to_json v.attribute_names)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; attribute_names =
        AttributeNameList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeNames"))
    ; message_attribute_names =
        MessageAttributeNameList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "MessageAttributeNames"))
    ; max_number_of_messages =
        Aws.Util.option_map (Aws.Json.lookup j "MaxNumberOfMessages") Integer.of_json
    ; visibility_timeout =
        Aws.Util.option_map (Aws.Json.lookup j "VisibilityTimeout") Integer.of_json
    ; wait_time_seconds =
        Aws.Util.option_map (Aws.Json.lookup j "WaitTimeSeconds") Integer.of_json
    ; receive_request_attempt_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReceiveRequestAttemptId") String.of_json
    }
end

module ReceiptHandleIsInvalid = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ChangeMessageVisibilityRequest = struct
  type t =
    { queue_url : String.t
    ; receipt_handle : String.t
    ; visibility_timeout : Integer.t
    }

  let make ~queue_url ~receipt_handle ~visibility_timeout () =
    { queue_url; receipt_handle; visibility_timeout }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; receipt_handle =
          Aws.Xml.required
            "ReceiptHandle"
            (Aws.Util.option_bind (Aws.Xml.member "ReceiptHandle" xml) String.parse)
      ; visibility_timeout =
          Aws.Xml.required
            "VisibilityTimeout"
            (Aws.Util.option_bind (Aws.Xml.member "VisibilityTimeout" xml) Integer.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("VisibilityTimeout", Integer.to_query v.visibility_timeout))
         ; Some (Aws.Query.Pair ("ReceiptHandle", String.to_query v.receipt_handle))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("VisibilityTimeout", Integer.to_json v.visibility_timeout)
         ; Some ("ReceiptHandle", String.to_json v.receipt_handle)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; receipt_handle =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReceiptHandle"))
    ; visibility_timeout =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VisibilityTimeout"))
    }
end

module ActionNameList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "ActionName" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module AddPermissionRequest = struct
  type t =
    { queue_url : String.t
    ; label : String.t
    ; a_w_s_account_ids : AWSAccountIdList.t
    ; actions : ActionNameList.t
    }

  let make ~queue_url ~label ~a_w_s_account_ids ~actions () =
    { queue_url; label; a_w_s_account_ids; actions }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; label =
          Aws.Xml.required
            "Label"
            (Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse)
      ; a_w_s_account_ids = Aws.Xml.required "AWSAccountIds" (AWSAccountIdList.parse xml)
      ; actions = Aws.Xml.required "Actions" (ActionNameList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Actions.member", ActionNameList.to_query v.actions))
         ; Some
             (Aws.Query.Pair
                ("AWSAccountIds.member", AWSAccountIdList.to_query v.a_w_s_account_ids))
         ; Some (Aws.Query.Pair ("Label", String.to_query v.label))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Actions", ActionNameList.to_json v.actions)
         ; Some ("AWSAccountIds", AWSAccountIdList.to_json v.a_w_s_account_ids)
         ; Some ("Label", String.to_json v.label)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; label = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Label"))
    ; a_w_s_account_ids =
        AWSAccountIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AWSAccountIds"))
    ; actions =
        ActionNameList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Actions"))
    }
end

module SendMessageBatchResultEntryList = struct
  type t = SendMessageBatchResultEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         SendMessageBatchResultEntry.parse
         (Aws.Xml.members "SendMessageBatchResultEntry" xml))

  let to_query v = Aws.Query.to_query_list SendMessageBatchResultEntry.to_query v

  let to_json v = `List (List.map SendMessageBatchResultEntry.to_json v)

  let of_json j = Aws.Json.to_list SendMessageBatchResultEntry.of_json j
end

module SendMessageBatchResult = struct
  type t =
    { successful : SendMessageBatchResultEntryList.t
    ; failed : BatchResultErrorEntryList.t
    }

  let make ~successful ~failed () = { successful; failed }

  let parse xml =
    Some
      { successful =
          Aws.Xml.required "Successful" (SendMessageBatchResultEntryList.parse xml)
      ; failed = Aws.Xml.required "Failed" (BatchResultErrorEntryList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("Failed.member", BatchResultErrorEntryList.to_query v.failed))
         ; Some
             (Aws.Query.Pair
                ( "Successful.member"
                , SendMessageBatchResultEntryList.to_query v.successful ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Failed", BatchResultErrorEntryList.to_json v.failed)
         ; Some ("Successful", SendMessageBatchResultEntryList.to_json v.successful)
         ])

  let of_json j =
    { successful =
        SendMessageBatchResultEntryList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Successful"))
    ; failed =
        BatchResultErrorEntryList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Failed"))
    }
end

module RemovePermissionRequest = struct
  type t =
    { queue_url : String.t
    ; label : String.t
    }

  let make ~queue_url ~label () = { queue_url; label }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; label =
          Aws.Xml.required
            "Label"
            (Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Label", String.to_query v.label))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Label", String.to_json v.label)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; label = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Label"))
    }
end

module ChangeMessageVisibilityBatchResult = struct
  type t =
    { successful : ChangeMessageVisibilityBatchResultEntryList.t
    ; failed : BatchResultErrorEntryList.t
    }

  let make ~successful ~failed () = { successful; failed }

  let parse xml =
    Some
      { successful =
          Aws.Xml.required
            "Successful"
            (ChangeMessageVisibilityBatchResultEntryList.parse xml)
      ; failed = Aws.Xml.required "Failed" (BatchResultErrorEntryList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("Failed.member", BatchResultErrorEntryList.to_query v.failed))
         ; Some
             (Aws.Query.Pair
                ( "Successful.member"
                , ChangeMessageVisibilityBatchResultEntryList.to_query v.successful ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Failed", BatchResultErrorEntryList.to_json v.failed)
         ; Some
             ( "Successful"
             , ChangeMessageVisibilityBatchResultEntryList.to_json v.successful )
         ])

  let of_json j =
    { successful =
        ChangeMessageVisibilityBatchResultEntryList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Successful"))
    ; failed =
        BatchResultErrorEntryList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Failed"))
    }
end

module DeleteMessageBatchRequest = struct
  type t =
    { queue_url : String.t
    ; entries : DeleteMessageBatchRequestEntryList.t
    }

  let make ~queue_url ~entries () = { queue_url; entries }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; entries =
          Aws.Xml.required "Entries" (DeleteMessageBatchRequestEntryList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("Entries.member", DeleteMessageBatchRequestEntryList.to_query v.entries))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Entries", DeleteMessageBatchRequestEntryList.to_json v.entries)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; entries =
        DeleteMessageBatchRequestEntryList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Entries"))
    }
end

module SendMessageBatchRequest = struct
  type t =
    { queue_url : String.t
    ; entries : SendMessageBatchRequestEntryList.t
    }

  let make ~queue_url ~entries () = { queue_url; entries }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; entries = Aws.Xml.required "Entries" (SendMessageBatchRequestEntryList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("Entries.member", SendMessageBatchRequestEntryList.to_query v.entries))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Entries", SendMessageBatchRequestEntryList.to_json v.entries)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; entries =
        SendMessageBatchRequestEntryList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Entries"))
    }
end

module ChangeMessageVisibilityBatchRequestEntryList = struct
  type t = ChangeMessageVisibilityBatchRequestEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         ChangeMessageVisibilityBatchRequestEntry.parse
         (Aws.Xml.members "ChangeMessageVisibilityBatchRequestEntry" xml))

  let to_query v =
    Aws.Query.to_query_list ChangeMessageVisibilityBatchRequestEntry.to_query v

  let to_json v = `List (List.map ChangeMessageVisibilityBatchRequestEntry.to_json v)

  let of_json j = Aws.Json.to_list ChangeMessageVisibilityBatchRequestEntry.of_json j
end

module ChangeMessageVisibilityBatchRequest = struct
  type t =
    { queue_url : String.t
    ; entries : ChangeMessageVisibilityBatchRequestEntryList.t
    }

  let make ~queue_url ~entries () = { queue_url; entries }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      ; entries =
          Aws.Xml.required
            "Entries"
            (ChangeMessageVisibilityBatchRequestEntryList.parse xml)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "Entries.member"
                , ChangeMessageVisibilityBatchRequestEntryList.to_query v.entries ))
         ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Entries", ChangeMessageVisibilityBatchRequestEntryList.to_json v.entries)
         ; Some ("QueueUrl", String.to_json v.queue_url)
         ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
    ; entries =
        ChangeMessageVisibilityBatchRequestEntryList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Entries"))
    }
end

module EmptyBatchRequest = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module PurgeQueueInProgress = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module GetQueueAttributesResult = struct
  type t = { attributes : QueueAttributeMap.t option }

  let make ?attributes () = { attributes }

  let parse xml =
    Some
      { attributes =
          Aws.Util.option_bind (Aws.Xml.member "Attribute" xml) QueueAttributeMap.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.attributes (fun f ->
               Aws.Query.Pair ("Attribute", QueueAttributeMap.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.attributes (fun f ->
               "Attribute", QueueAttributeMap.to_json f)
         ])

  let of_json j =
    { attributes =
        Aws.Util.option_map (Aws.Json.lookup j "Attribute") QueueAttributeMap.of_json
    }
end

module GetQueueUrlRequest = struct
  type t =
    { queue_name : String.t
    ; queue_owner_a_w_s_account_id : String.t option
    }

  let make ~queue_name ?queue_owner_a_w_s_account_id () =
    { queue_name; queue_owner_a_w_s_account_id }

  let parse xml =
    Some
      { queue_name =
          Aws.Xml.required
            "QueueName"
            (Aws.Util.option_bind (Aws.Xml.member "QueueName" xml) String.parse)
      ; queue_owner_a_w_s_account_id =
          Aws.Util.option_bind (Aws.Xml.member "QueueOwnerAWSAccountId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.queue_owner_a_w_s_account_id (fun f ->
               Aws.Query.Pair ("QueueOwnerAWSAccountId", String.to_query f))
         ; Some (Aws.Query.Pair ("QueueName", String.to_query v.queue_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.queue_owner_a_w_s_account_id (fun f ->
               "QueueOwnerAWSAccountId", String.to_json f)
         ; Some ("QueueName", String.to_json v.queue_name)
         ])

  let of_json j =
    { queue_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueName"))
    ; queue_owner_a_w_s_account_id =
        Aws.Util.option_map (Aws.Json.lookup j "QueueOwnerAWSAccountId") String.of_json
    }
end

module InvalidIdFormat = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteQueueRequest = struct
  type t = { queue_url : String.t }

  let make ~queue_url () = { queue_url }

  let parse xml =
    Some
      { queue_url =
          Aws.Xml.required
            "QueueUrl"
            (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("QueueUrl", String.to_json v.queue_url) ])

  let of_json j =
    { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl")) }
end

module MessageNotInflight = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end
