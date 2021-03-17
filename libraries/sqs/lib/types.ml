open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module BinaryList =
  struct
    type t = Blob.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map Blob.parse (Xml.members "BinaryListValue" xml))
    let to_query v = Query.to_query_list Blob.to_query v
    let to_json v = `List (List.map Blob.to_json v)
    let of_json j = Json.to_list Blob.of_json j
  end
module StringList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "StringListValue" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module MessageAttributeValue =
  struct
    type t =
      {
      string_value: String.t option ;
      binary_value: Blob.t option ;
      string_list_values: StringList.t ;
      binary_list_values: BinaryList.t ;
      data_type: String.t }
    let make ?string_value  ?binary_value  ?(string_list_values= []) 
      ?(binary_list_values= [])  ~data_type  () =
      {
        string_value;
        binary_value;
        string_list_values;
        binary_list_values;
        data_type
      }
    let parse xml =
      Some
        {
          string_value =
            (Util.option_bind (Xml.member "StringValue" xml) String.parse);
          binary_value =
            (Util.option_bind (Xml.member "BinaryValue" xml) Blob.parse);
          string_list_values =
            (Util.of_option []
               (Util.option_bind (Xml.member "StringListValue" xml)
                  StringList.parse));
          binary_list_values =
            (Util.of_option []
               (Util.option_bind (Xml.member "BinaryListValue" xml)
                  BinaryList.parse));
          data_type =
            (Xml.required "DataType"
               (Util.option_bind (Xml.member "DataType" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("DataType", (String.to_query v.data_type)));
           Some
             (Query.Pair
                ("BinaryListValue",
                  (BinaryList.to_query v.binary_list_values)));
           Some
             (Query.Pair
                ("StringListValue",
                  (StringList.to_query v.string_list_values)));
           Util.option_map v.binary_value
             (fun f -> Query.Pair ("BinaryValue", (Blob.to_query f)));
           Util.option_map v.string_value
             (fun f -> Query.Pair ("StringValue", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("data_type", (String.to_json v.data_type));
           Some
             ("binary_list_values",
               (BinaryList.to_json v.binary_list_values));
           Some
             ("string_list_values",
               (StringList.to_json v.string_list_values));
           Util.option_map v.binary_value
             (fun f -> ("binary_value", (Blob.to_json f)));
           Util.option_map v.string_value
             (fun f -> ("string_value", (String.to_json f)))])
    let of_json j =
      {
        string_value =
          (Util.option_map (Json.lookup j "string_value") String.of_json);
        binary_value =
          (Util.option_map (Json.lookup j "binary_value") Blob.of_json);
        string_list_values =
          (StringList.of_json
             (Util.of_option_exn (Json.lookup j "string_list_values")));
        binary_list_values =
          (BinaryList.of_json
             (Util.of_option_exn (Json.lookup j "binary_list_values")));
        data_type =
          (String.of_json (Util.of_option_exn (Json.lookup j "data_type")))
      }
  end
module MessageSystemAttributeNameForSends =
  struct
    type t =
      | AWSTraceHeader 
    let str_to_t = [("AWSTraceHeader", AWSTraceHeader)]
    let t_to_str = [(AWSTraceHeader, "AWSTraceHeader")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module MessageSystemAttributeValue =
  struct
    type t =
      {
      string_value: String.t option ;
      binary_value: Blob.t option ;
      string_list_values: StringList.t ;
      binary_list_values: BinaryList.t ;
      data_type: String.t }
    let make ?string_value  ?binary_value  ?(string_list_values= []) 
      ?(binary_list_values= [])  ~data_type  () =
      {
        string_value;
        binary_value;
        string_list_values;
        binary_list_values;
        data_type
      }
    let parse xml =
      Some
        {
          string_value =
            (Util.option_bind (Xml.member "StringValue" xml) String.parse);
          binary_value =
            (Util.option_bind (Xml.member "BinaryValue" xml) Blob.parse);
          string_list_values =
            (Util.of_option []
               (Util.option_bind (Xml.member "StringListValue" xml)
                  StringList.parse));
          binary_list_values =
            (Util.of_option []
               (Util.option_bind (Xml.member "BinaryListValue" xml)
                  BinaryList.parse));
          data_type =
            (Xml.required "DataType"
               (Util.option_bind (Xml.member "DataType" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("DataType", (String.to_query v.data_type)));
           Some
             (Query.Pair
                ("BinaryListValue",
                  (BinaryList.to_query v.binary_list_values)));
           Some
             (Query.Pair
                ("StringListValue",
                  (StringList.to_query v.string_list_values)));
           Util.option_map v.binary_value
             (fun f -> Query.Pair ("BinaryValue", (Blob.to_query f)));
           Util.option_map v.string_value
             (fun f -> Query.Pair ("StringValue", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("data_type", (String.to_json v.data_type));
           Some
             ("binary_list_values",
               (BinaryList.to_json v.binary_list_values));
           Some
             ("string_list_values",
               (StringList.to_json v.string_list_values));
           Util.option_map v.binary_value
             (fun f -> ("binary_value", (Blob.to_json f)));
           Util.option_map v.string_value
             (fun f -> ("string_value", (String.to_json f)))])
    let of_json j =
      {
        string_value =
          (Util.option_map (Json.lookup j "string_value") String.of_json);
        binary_value =
          (Util.option_map (Json.lookup j "binary_value") Blob.of_json);
        string_list_values =
          (StringList.of_json
             (Util.of_option_exn (Json.lookup j "string_list_values")));
        binary_list_values =
          (BinaryList.of_json
             (Util.of_option_exn (Json.lookup j "binary_list_values")));
        data_type =
          (String.of_json (Util.of_option_exn (Json.lookup j "data_type")))
      }
  end
module MessageSystemAttributeName =
  struct
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
      [("AWSTraceHeader", AWSTraceHeader);
      ("MessageGroupId", MessageGroupId);
      ("MessageDeduplicationId", MessageDeduplicationId);
      ("SequenceNumber", SequenceNumber);
      ("ApproximateFirstReceiveTimestamp", ApproximateFirstReceiveTimestamp);
      ("ApproximateReceiveCount", ApproximateReceiveCount);
      ("SentTimestamp", SentTimestamp);
      ("SenderId", SenderId)]
    let t_to_str =
      [(AWSTraceHeader, "AWSTraceHeader");
      (MessageGroupId, "MessageGroupId");
      (MessageDeduplicationId, "MessageDeduplicationId");
      (SequenceNumber, "SequenceNumber");
      (ApproximateFirstReceiveTimestamp, "ApproximateFirstReceiveTimestamp");
      (ApproximateReceiveCount, "ApproximateReceiveCount");
      (SentTimestamp, "SentTimestamp");
      (SenderId, "SenderId")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module MessageBodyAttributeMap =
  struct
    type t = (String.t, MessageAttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string MessageAttributeValue.to_query
        v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k), (MessageAttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Json.to_hashtbl String.of_string MessageAttributeValue.of_json j
  end
module MessageBodySystemAttributeMap =
  struct
    type t =
      (MessageSystemAttributeNameForSends.t, MessageSystemAttributeValue.t)
        Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl MessageSystemAttributeNameForSends.to_string
        MessageSystemAttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((MessageSystemAttributeNameForSends.to_string k),
                    (MessageSystemAttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Json.to_hashtbl MessageSystemAttributeNameForSends.of_string
        MessageSystemAttributeValue.of_json j
  end
module MessageSystemAttributeMap =
  struct
    type t = (MessageSystemAttributeName.t, String.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl MessageSystemAttributeName.to_string
        String.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((MessageSystemAttributeName.to_string k),
                    (String.to_json v))
                  :: acc) v [])
    let of_json j =
      Json.to_hashtbl MessageSystemAttributeName.of_string String.of_json j
  end
module QueueAttributeName =
  struct
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
      [("KmsDataKeyReusePeriodSeconds", KmsDataKeyReusePeriodSeconds);
      ("KmsMasterKeyId", KmsMasterKeyId);
      ("ContentBasedDeduplication", ContentBasedDeduplication);
      ("FifoQueue", FifoQueue);
      ("RedrivePolicy", RedrivePolicy);
      ("ReceiveMessageWaitTimeSeconds", ReceiveMessageWaitTimeSeconds);
      ("DelaySeconds", DelaySeconds);
      ("ApproximateNumberOfMessagesDelayed",
        ApproximateNumberOfMessagesDelayed);
      ("QueueArn", QueueArn);
      ("LastModifiedTimestamp", LastModifiedTimestamp);
      ("CreatedTimestamp", CreatedTimestamp);
      ("ApproximateNumberOfMessagesNotVisible",
        ApproximateNumberOfMessagesNotVisible);
      ("ApproximateNumberOfMessages", ApproximateNumberOfMessages);
      ("MessageRetentionPeriod", MessageRetentionPeriod);
      ("MaximumMessageSize", MaximumMessageSize);
      ("VisibilityTimeout", VisibilityTimeout);
      ("Policy", Policy);
      ("All", All)]
    let t_to_str =
      [(KmsDataKeyReusePeriodSeconds, "KmsDataKeyReusePeriodSeconds");
      (KmsMasterKeyId, "KmsMasterKeyId");
      (ContentBasedDeduplication, "ContentBasedDeduplication");
      (FifoQueue, "FifoQueue");
      (RedrivePolicy, "RedrivePolicy");
      (ReceiveMessageWaitTimeSeconds, "ReceiveMessageWaitTimeSeconds");
      (DelaySeconds, "DelaySeconds");
      (ApproximateNumberOfMessagesDelayed,
        "ApproximateNumberOfMessagesDelayed");
      (QueueArn, "QueueArn");
      (LastModifiedTimestamp, "LastModifiedTimestamp");
      (CreatedTimestamp, "CreatedTimestamp");
      (ApproximateNumberOfMessagesNotVisible,
        "ApproximateNumberOfMessagesNotVisible");
      (ApproximateNumberOfMessages, "ApproximateNumberOfMessages");
      (MessageRetentionPeriod, "MessageRetentionPeriod");
      (MaximumMessageSize, "MaximumMessageSize");
      (VisibilityTimeout, "VisibilityTimeout");
      (Policy, "Policy");
      (All, "All")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ChangeMessageVisibilityBatchRequestEntry =
  struct
    type t =
      {
      id: String.t ;
      receipt_handle: String.t ;
      visibility_timeout: Integer.t option }
    let make ~id  ~receipt_handle  ?visibility_timeout  () =
      { id; receipt_handle; visibility_timeout }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          receipt_handle =
            (Xml.required "ReceiptHandle"
               (Util.option_bind (Xml.member "ReceiptHandle" xml)
                  String.parse));
          visibility_timeout =
            (Util.option_bind (Xml.member "VisibilityTimeout" xml)
               Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.visibility_timeout
              (fun f ->
                 Query.Pair ("VisibilityTimeout", (Integer.to_query f)));
           Some
             (Query.Pair
                ("ReceiptHandle", (String.to_query v.receipt_handle)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.visibility_timeout
              (fun f -> ("visibility_timeout", (Integer.to_json f)));
           Some ("receipt_handle", (String.to_json v.receipt_handle));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        receipt_handle =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "receipt_handle")));
        visibility_timeout =
          (Util.option_map (Json.lookup j "visibility_timeout")
             Integer.of_json)
      }
  end
module SendMessageBatchRequestEntry =
  struct
    type t =
      {
      id: String.t ;
      message_body: String.t ;
      delay_seconds: Integer.t option ;
      message_attributes: MessageBodyAttributeMap.t option ;
      message_system_attributes: MessageBodySystemAttributeMap.t option ;
      message_deduplication_id: String.t option ;
      message_group_id: String.t option }
    let make ~id  ~message_body  ?delay_seconds  ?message_attributes 
      ?message_system_attributes  ?message_deduplication_id 
      ?message_group_id  () =
      {
        id;
        message_body;
        delay_seconds;
        message_attributes;
        message_system_attributes;
        message_deduplication_id;
        message_group_id
      }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          message_body =
            (Xml.required "MessageBody"
               (Util.option_bind (Xml.member "MessageBody" xml) String.parse));
          delay_seconds =
            (Util.option_bind (Xml.member "DelaySeconds" xml) Integer.parse);
          message_attributes =
            (Util.option_bind (Xml.member "MessageAttribute" xml)
               MessageBodyAttributeMap.parse);
          message_system_attributes =
            (Util.option_bind (Xml.member "MessageSystemAttribute" xml)
               MessageBodySystemAttributeMap.parse);
          message_deduplication_id =
            (Util.option_bind (Xml.member "MessageDeduplicationId" xml)
               String.parse);
          message_group_id =
            (Util.option_bind (Xml.member "MessageGroupId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message_group_id
              (fun f -> Query.Pair ("MessageGroupId", (String.to_query f)));
           Util.option_map v.message_deduplication_id
             (fun f ->
                Query.Pair ("MessageDeduplicationId", (String.to_query f)));
           Util.option_map v.message_system_attributes
             (fun f ->
                Query.Pair
                  ("MessageSystemAttribute",
                    (MessageBodySystemAttributeMap.to_query f)));
           Util.option_map v.message_attributes
             (fun f ->
                Query.Pair
                  ("MessageAttribute", (MessageBodyAttributeMap.to_query f)));
           Util.option_map v.delay_seconds
             (fun f -> Query.Pair ("DelaySeconds", (Integer.to_query f)));
           Some
             (Query.Pair ("MessageBody", (String.to_query v.message_body)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message_group_id
              (fun f -> ("message_group_id", (String.to_json f)));
           Util.option_map v.message_deduplication_id
             (fun f -> ("message_deduplication_id", (String.to_json f)));
           Util.option_map v.message_system_attributes
             (fun f ->
                ("message_system_attributes",
                  (MessageBodySystemAttributeMap.to_json f)));
           Util.option_map v.message_attributes
             (fun f ->
                ("message_attributes", (MessageBodyAttributeMap.to_json f)));
           Util.option_map v.delay_seconds
             (fun f -> ("delay_seconds", (Integer.to_json f)));
           Some ("message_body", (String.to_json v.message_body));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        message_body =
          (String.of_json (Util.of_option_exn (Json.lookup j "message_body")));
        delay_seconds =
          (Util.option_map (Json.lookup j "delay_seconds") Integer.of_json);
        message_attributes =
          (Util.option_map (Json.lookup j "message_attributes")
             MessageBodyAttributeMap.of_json);
        message_system_attributes =
          (Util.option_map (Json.lookup j "message_system_attributes")
             MessageBodySystemAttributeMap.of_json);
        message_deduplication_id =
          (Util.option_map (Json.lookup j "message_deduplication_id")
             String.of_json);
        message_group_id =
          (Util.option_map (Json.lookup j "message_group_id") String.of_json)
      }
  end
module DeleteMessageBatchRequestEntry =
  struct
    type t = {
      id: String.t ;
      receipt_handle: String.t }
    let make ~id  ~receipt_handle  () = { id; receipt_handle }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          receipt_handle =
            (Xml.required "ReceiptHandle"
               (Util.option_bind (Xml.member "ReceiptHandle" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ReceiptHandle", (String.to_query v.receipt_handle)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("receipt_handle", (String.to_json v.receipt_handle));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        receipt_handle =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "receipt_handle")))
      }
  end
module BatchResultErrorEntry =
  struct
    type t =
      {
      id: String.t ;
      sender_fault: Boolean.t ;
      code: String.t ;
      message: String.t option }
    let make ~id  ~sender_fault  ~code  ?message  () =
      { id; sender_fault; code; message }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          sender_fault =
            (Xml.required "SenderFault"
               (Util.option_bind (Xml.member "SenderFault" xml) Boolean.parse));
          code =
            (Xml.required "Code"
               (Util.option_bind (Xml.member "Code" xml) String.parse));
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)));
           Some (Query.Pair ("Code", (String.to_query v.code)));
           Some
             (Query.Pair ("SenderFault", (Boolean.to_query v.sender_fault)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Some ("code", (String.to_json v.code));
           Some ("sender_fault", (Boolean.to_json v.sender_fault));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        sender_fault =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "sender_fault")));
        code = (String.of_json (Util.of_option_exn (Json.lookup j "code")));
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ChangeMessageVisibilityBatchResultEntry =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module SendMessageBatchResultEntry =
  struct
    type t =
      {
      id: String.t ;
      message_id: String.t ;
      m_d5_of_message_body: String.t ;
      m_d5_of_message_attributes: String.t option ;
      m_d5_of_message_system_attributes: String.t option ;
      sequence_number: String.t option }
    let make ~id  ~message_id  ~m_d5_of_message_body 
      ?m_d5_of_message_attributes  ?m_d5_of_message_system_attributes 
      ?sequence_number  () =
      {
        id;
        message_id;
        m_d5_of_message_body;
        m_d5_of_message_attributes;
        m_d5_of_message_system_attributes;
        sequence_number
      }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          message_id =
            (Xml.required "MessageId"
               (Util.option_bind (Xml.member "MessageId" xml) String.parse));
          m_d5_of_message_body =
            (Xml.required "MD5OfMessageBody"
               (Util.option_bind (Xml.member "MD5OfMessageBody" xml)
                  String.parse));
          m_d5_of_message_attributes =
            (Util.option_bind (Xml.member "MD5OfMessageAttributes" xml)
               String.parse);
          m_d5_of_message_system_attributes =
            (Util.option_bind (Xml.member "MD5OfMessageSystemAttributes" xml)
               String.parse);
          sequence_number =
            (Util.option_bind (Xml.member "SequenceNumber" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.sequence_number
              (fun f -> Query.Pair ("SequenceNumber", (String.to_query f)));
           Util.option_map v.m_d5_of_message_system_attributes
             (fun f ->
                Query.Pair
                  ("MD5OfMessageSystemAttributes", (String.to_query f)));
           Util.option_map v.m_d5_of_message_attributes
             (fun f ->
                Query.Pair ("MD5OfMessageAttributes", (String.to_query f)));
           Some
             (Query.Pair
                ("MD5OfMessageBody",
                  (String.to_query v.m_d5_of_message_body)));
           Some (Query.Pair ("MessageId", (String.to_query v.message_id)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.sequence_number
              (fun f -> ("sequence_number", (String.to_json f)));
           Util.option_map v.m_d5_of_message_system_attributes
             (fun f ->
                ("m_d5_of_message_system_attributes", (String.to_json f)));
           Util.option_map v.m_d5_of_message_attributes
             (fun f -> ("m_d5_of_message_attributes", (String.to_json f)));
           Some
             ("m_d5_of_message_body",
               (String.to_json v.m_d5_of_message_body));
           Some ("message_id", (String.to_json v.message_id));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        message_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "message_id")));
        m_d5_of_message_body =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "m_d5_of_message_body")));
        m_d5_of_message_attributes =
          (Util.option_map (Json.lookup j "m_d5_of_message_attributes")
             String.of_json);
        m_d5_of_message_system_attributes =
          (Util.option_map
             (Json.lookup j "m_d5_of_message_system_attributes")
             String.of_json);
        sequence_number =
          (Util.option_map (Json.lookup j "sequence_number") String.of_json)
      }
  end
module Message =
  struct
    type t =
      {
      message_id: String.t option ;
      receipt_handle: String.t option ;
      m_d5_of_body: String.t option ;
      body: String.t option ;
      attributes: MessageSystemAttributeMap.t option ;
      m_d5_of_message_attributes: String.t option ;
      message_attributes: MessageBodyAttributeMap.t option }
    let make ?message_id  ?receipt_handle  ?m_d5_of_body  ?body  ?attributes 
      ?m_d5_of_message_attributes  ?message_attributes  () =
      {
        message_id;
        receipt_handle;
        m_d5_of_body;
        body;
        attributes;
        m_d5_of_message_attributes;
        message_attributes
      }
    let parse xml =
      Some
        {
          message_id =
            (Util.option_bind (Xml.member "MessageId" xml) String.parse);
          receipt_handle =
            (Util.option_bind (Xml.member "ReceiptHandle" xml) String.parse);
          m_d5_of_body =
            (Util.option_bind (Xml.member "MD5OfBody" xml) String.parse);
          body = (Util.option_bind (Xml.member "Body" xml) String.parse);
          attributes =
            (Util.option_bind (Xml.member "Attribute" xml)
               MessageSystemAttributeMap.parse);
          m_d5_of_message_attributes =
            (Util.option_bind (Xml.member "MD5OfMessageAttributes" xml)
               String.parse);
          message_attributes =
            (Util.option_bind (Xml.member "MessageAttribute" xml)
               MessageBodyAttributeMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message_attributes
              (fun f ->
                 Query.Pair
                   ("MessageAttribute", (MessageBodyAttributeMap.to_query f)));
           Util.option_map v.m_d5_of_message_attributes
             (fun f ->
                Query.Pair ("MD5OfMessageAttributes", (String.to_query f)));
           Util.option_map v.attributes
             (fun f ->
                Query.Pair
                  ("Attribute", (MessageSystemAttributeMap.to_query f)));
           Util.option_map v.body
             (fun f -> Query.Pair ("Body", (String.to_query f)));
           Util.option_map v.m_d5_of_body
             (fun f -> Query.Pair ("MD5OfBody", (String.to_query f)));
           Util.option_map v.receipt_handle
             (fun f -> Query.Pair ("ReceiptHandle", (String.to_query f)));
           Util.option_map v.message_id
             (fun f -> Query.Pair ("MessageId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message_attributes
              (fun f ->
                 ("message_attributes", (MessageBodyAttributeMap.to_json f)));
           Util.option_map v.m_d5_of_message_attributes
             (fun f -> ("m_d5_of_message_attributes", (String.to_json f)));
           Util.option_map v.attributes
             (fun f -> ("attributes", (MessageSystemAttributeMap.to_json f)));
           Util.option_map v.body (fun f -> ("body", (String.to_json f)));
           Util.option_map v.m_d5_of_body
             (fun f -> ("m_d5_of_body", (String.to_json f)));
           Util.option_map v.receipt_handle
             (fun f -> ("receipt_handle", (String.to_json f)));
           Util.option_map v.message_id
             (fun f -> ("message_id", (String.to_json f)))])
    let of_json j =
      {
        message_id =
          (Util.option_map (Json.lookup j "message_id") String.of_json);
        receipt_handle =
          (Util.option_map (Json.lookup j "receipt_handle") String.of_json);
        m_d5_of_body =
          (Util.option_map (Json.lookup j "m_d5_of_body") String.of_json);
        body = (Util.option_map (Json.lookup j "body") String.of_json);
        attributes =
          (Util.option_map (Json.lookup j "attributes")
             MessageSystemAttributeMap.of_json);
        m_d5_of_message_attributes =
          (Util.option_map (Json.lookup j "m_d5_of_message_attributes")
             String.of_json);
        message_attributes =
          (Util.option_map (Json.lookup j "message_attributes")
             MessageBodyAttributeMap.of_json)
      }
  end
module DeleteMessageBatchResultEntry =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module QueueAttributeMap =
  struct
    type t = (QueueAttributeName.t, String.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl QueueAttributeName.to_string String.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((QueueAttributeName.to_string k), (String.to_json v)) ::
                  acc) v [])
    let of_json j =
      Json.to_hashtbl QueueAttributeName.of_string String.of_json j
  end
module ChangeMessageVisibilityBatchRequestEntryList =
  struct
    type t = ChangeMessageVisibilityBatchRequestEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ChangeMessageVisibilityBatchRequestEntry.parse
           (Xml.members "ChangeMessageVisibilityBatchRequestEntry" xml))
    let to_query v =
      Query.to_query_list ChangeMessageVisibilityBatchRequestEntry.to_query v
    let to_json v =
      `List (List.map ChangeMessageVisibilityBatchRequestEntry.to_json v)
    let of_json j =
      Json.to_list ChangeMessageVisibilityBatchRequestEntry.of_json j
  end
module SendMessageBatchRequestEntryList =
  struct
    type t = SendMessageBatchRequestEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map SendMessageBatchRequestEntry.parse
           (Xml.members "SendMessageBatchRequestEntry" xml))
    let to_query v =
      Query.to_query_list SendMessageBatchRequestEntry.to_query v
    let to_json v = `List (List.map SendMessageBatchRequestEntry.to_json v)
    let of_json j = Json.to_list SendMessageBatchRequestEntry.of_json j
  end
module DeleteMessageBatchRequestEntryList =
  struct
    type t = DeleteMessageBatchRequestEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DeleteMessageBatchRequestEntry.parse
           (Xml.members "DeleteMessageBatchRequestEntry" xml))
    let to_query v =
      Query.to_query_list DeleteMessageBatchRequestEntry.to_query v
    let to_json v = `List (List.map DeleteMessageBatchRequestEntry.to_json v)
    let of_json j = Json.to_list DeleteMessageBatchRequestEntry.of_json j
  end
module BatchResultErrorEntryList =
  struct
    type t = BatchResultErrorEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map BatchResultErrorEntry.parse
           (Xml.members "BatchResultErrorEntry" xml))
    let to_query v = Query.to_query_list BatchResultErrorEntry.to_query v
    let to_json v = `List (List.map BatchResultErrorEntry.to_json v)
    let of_json j = Json.to_list BatchResultErrorEntry.of_json j
  end
module ChangeMessageVisibilityBatchResultEntryList =
  struct
    type t = ChangeMessageVisibilityBatchResultEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ChangeMessageVisibilityBatchResultEntry.parse
           (Xml.members "ChangeMessageVisibilityBatchResultEntry" xml))
    let to_query v =
      Query.to_query_list ChangeMessageVisibilityBatchResultEntry.to_query v
    let to_json v =
      `List (List.map ChangeMessageVisibilityBatchResultEntry.to_json v)
    let of_json j =
      Json.to_list ChangeMessageVisibilityBatchResultEntry.of_json j
  end
module SendMessageBatchResultEntryList =
  struct
    type t = SendMessageBatchResultEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map SendMessageBatchResultEntry.parse
           (Xml.members "SendMessageBatchResultEntry" xml))
    let to_query v =
      Query.to_query_list SendMessageBatchResultEntry.to_query v
    let to_json v = `List (List.map SendMessageBatchResultEntry.to_json v)
    let of_json j = Json.to_list SendMessageBatchResultEntry.of_json j
  end
module AWSAccountIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "AWSAccountId" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ActionNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "ActionName" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module AttributeNameList =
  struct
    type t = QueueAttributeName.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map QueueAttributeName.parse (Xml.members "AttributeName" xml))
    let to_query v = Query.to_query_list QueueAttributeName.to_query v
    let to_json v = `List (List.map QueueAttributeName.to_json v)
    let of_json j = Json.to_list QueueAttributeName.of_json j
  end
module MessageAttributeNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "MessageAttributeName" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module QueueUrlList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "QueueUrl" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module TagMap =
  struct
    type t = (String.t, String.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string String.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (String.to_json v)) :: acc)
           v [])
    let of_json j = Json.to_hashtbl String.of_string String.of_json j
  end
module MessageList =
  struct
    type t = Message.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Message.parse (Xml.members "Message" xml))
    let to_query v = Query.to_query_list Message.to_query v
    let to_json v = `List (List.map Message.to_json v)
    let of_json j = Json.to_list Message.of_json j
  end
module DeleteMessageBatchResultEntryList =
  struct
    type t = DeleteMessageBatchResultEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DeleteMessageBatchResultEntry.parse
           (Xml.members "DeleteMessageBatchResultEntry" xml))
    let to_query v =
      Query.to_query_list DeleteMessageBatchResultEntry.to_query v
    let to_json v = `List (List.map DeleteMessageBatchResultEntry.to_json v)
    let of_json j = Json.to_list DeleteMessageBatchResultEntry.of_json j
  end
module TagKeyList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "TagKey" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module MessageNotInflight =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteQueueRequest =
  struct
    type t = {
      queue_url: String.t }
    let make ~queue_url  () = { queue_url }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")))
      }
  end
module InvalidIdFormat =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GetQueueUrlRequest =
  struct
    type t =
      {
      queue_name: String.t ;
      queue_owner_a_w_s_account_id: String.t option }
    let make ~queue_name  ?queue_owner_a_w_s_account_id  () =
      { queue_name; queue_owner_a_w_s_account_id }
    let parse xml =
      Some
        {
          queue_name =
            (Xml.required "QueueName"
               (Util.option_bind (Xml.member "QueueName" xml) String.parse));
          queue_owner_a_w_s_account_id =
            (Util.option_bind (Xml.member "QueueOwnerAWSAccountId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.queue_owner_a_w_s_account_id
              (fun f ->
                 Query.Pair ("QueueOwnerAWSAccountId", (String.to_query f)));
           Some (Query.Pair ("QueueName", (String.to_query v.queue_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.queue_owner_a_w_s_account_id
              (fun f -> ("queue_owner_a_w_s_account_id", (String.to_json f)));
           Some ("queue_name", (String.to_json v.queue_name))])
    let of_json j =
      {
        queue_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_name")));
        queue_owner_a_w_s_account_id =
          (Util.option_map (Json.lookup j "queue_owner_a_w_s_account_id")
             String.of_json)
      }
  end
module GetQueueAttributesResult =
  struct
    type t = {
      attributes: QueueAttributeMap.t option }
    let make ?attributes  () = { attributes }
    let parse xml =
      Some
        {
          attributes =
            (Util.option_bind (Xml.member "Attribute" xml)
               QueueAttributeMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.attributes
              (fun f ->
                 Query.Pair ("Attribute", (QueueAttributeMap.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.attributes
              (fun f -> ("attributes", (QueueAttributeMap.to_json f)))])
    let of_json j =
      {
        attributes =
          (Util.option_map (Json.lookup j "attributes")
             QueueAttributeMap.of_json)
      }
  end
module PurgeQueueInProgress =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module EmptyBatchRequest =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ChangeMessageVisibilityBatchRequest =
  struct
    type t =
      {
      queue_url: String.t ;
      entries: ChangeMessageVisibilityBatchRequestEntryList.t }
    let make ~queue_url  ~entries  () = { queue_url; entries }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          entries =
            (Xml.required "Entries"
               (ChangeMessageVisibilityBatchRequestEntryList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Entries.member",
                   (ChangeMessageVisibilityBatchRequestEntryList.to_query
                      v.entries)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("entries",
                (ChangeMessageVisibilityBatchRequestEntryList.to_json
                   v.entries));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        entries =
          (ChangeMessageVisibilityBatchRequestEntryList.of_json
             (Util.of_option_exn (Json.lookup j "entries")))
      }
  end
module SendMessageBatchRequest =
  struct
    type t =
      {
      queue_url: String.t ;
      entries: SendMessageBatchRequestEntryList.t }
    let make ~queue_url  ~entries  () = { queue_url; entries }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          entries =
            (Xml.required "Entries"
               (SendMessageBatchRequestEntryList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Entries.member",
                   (SendMessageBatchRequestEntryList.to_query v.entries)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("entries",
                (SendMessageBatchRequestEntryList.to_json v.entries));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        entries =
          (SendMessageBatchRequestEntryList.of_json
             (Util.of_option_exn (Json.lookup j "entries")))
      }
  end
module DeleteMessageBatchRequest =
  struct
    type t =
      {
      queue_url: String.t ;
      entries: DeleteMessageBatchRequestEntryList.t }
    let make ~queue_url  ~entries  () = { queue_url; entries }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          entries =
            (Xml.required "Entries"
               (DeleteMessageBatchRequestEntryList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Entries.member",
                   (DeleteMessageBatchRequestEntryList.to_query v.entries)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("entries",
                (DeleteMessageBatchRequestEntryList.to_json v.entries));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        entries =
          (DeleteMessageBatchRequestEntryList.of_json
             (Util.of_option_exn (Json.lookup j "entries")))
      }
  end
module ChangeMessageVisibilityBatchResult =
  struct
    type t =
      {
      successful: ChangeMessageVisibilityBatchResultEntryList.t ;
      failed: BatchResultErrorEntryList.t }
    let make ~successful  ~failed  () = { successful; failed }
    let parse xml =
      Some
        {
          successful =
            (Xml.required "Successful"
               (ChangeMessageVisibilityBatchResultEntryList.parse xml));
          failed =
            (Xml.required "Failed" (BatchResultErrorEntryList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Failed.member",
                   (BatchResultErrorEntryList.to_query v.failed)));
           Some
             (Query.Pair
                ("Successful.member",
                  (ChangeMessageVisibilityBatchResultEntryList.to_query
                     v.successful)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("failed", (BatchResultErrorEntryList.to_json v.failed));
           Some
             ("successful",
               (ChangeMessageVisibilityBatchResultEntryList.to_json
                  v.successful))])
    let of_json j =
      {
        successful =
          (ChangeMessageVisibilityBatchResultEntryList.of_json
             (Util.of_option_exn (Json.lookup j "successful")));
        failed =
          (BatchResultErrorEntryList.of_json
             (Util.of_option_exn (Json.lookup j "failed")))
      }
  end
module RemovePermissionRequest =
  struct
    type t = {
      queue_url: String.t ;
      label: String.t }
    let make ~queue_url  ~label  () = { queue_url; label }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          label =
            (Xml.required "Label"
               (Util.option_bind (Xml.member "Label" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Label", (String.to_query v.label)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("label", (String.to_json v.label));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        label = (String.of_json (Util.of_option_exn (Json.lookup j "label")))
      }
  end
module SendMessageBatchResult =
  struct
    type t =
      {
      successful: SendMessageBatchResultEntryList.t ;
      failed: BatchResultErrorEntryList.t }
    let make ~successful  ~failed  () = { successful; failed }
    let parse xml =
      Some
        {
          successful =
            (Xml.required "Successful"
               (SendMessageBatchResultEntryList.parse xml));
          failed =
            (Xml.required "Failed" (BatchResultErrorEntryList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Failed.member",
                   (BatchResultErrorEntryList.to_query v.failed)));
           Some
             (Query.Pair
                ("Successful.member",
                  (SendMessageBatchResultEntryList.to_query v.successful)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("failed", (BatchResultErrorEntryList.to_json v.failed));
           Some
             ("successful",
               (SendMessageBatchResultEntryList.to_json v.successful))])
    let of_json j =
      {
        successful =
          (SendMessageBatchResultEntryList.of_json
             (Util.of_option_exn (Json.lookup j "successful")));
        failed =
          (BatchResultErrorEntryList.of_json
             (Util.of_option_exn (Json.lookup j "failed")))
      }
  end
module AddPermissionRequest =
  struct
    type t =
      {
      queue_url: String.t ;
      label: String.t ;
      a_w_s_account_ids: AWSAccountIdList.t ;
      actions: ActionNameList.t }
    let make ~queue_url  ~label  ~a_w_s_account_ids  ~actions  () =
      { queue_url; label; a_w_s_account_ids; actions }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          label =
            (Xml.required "Label"
               (Util.option_bind (Xml.member "Label" xml) String.parse));
          a_w_s_account_ids =
            (Xml.required "AWSAccountIds" (AWSAccountIdList.parse xml));
          actions = (Xml.required "Actions" (ActionNameList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Actions.member", (ActionNameList.to_query v.actions)));
           Some
             (Query.Pair
                ("AWSAccountIds.member",
                  (AWSAccountIdList.to_query v.a_w_s_account_ids)));
           Some (Query.Pair ("Label", (String.to_query v.label)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("actions", (ActionNameList.to_json v.actions));
           Some
             ("a_w_s_account_ids",
               (AWSAccountIdList.to_json v.a_w_s_account_ids));
           Some ("label", (String.to_json v.label));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        label = (String.of_json (Util.of_option_exn (Json.lookup j "label")));
        a_w_s_account_ids =
          (AWSAccountIdList.of_json
             (Util.of_option_exn (Json.lookup j "a_w_s_account_ids")));
        actions =
          (ActionNameList.of_json
             (Util.of_option_exn (Json.lookup j "actions")))
      }
  end
module ChangeMessageVisibilityRequest =
  struct
    type t =
      {
      queue_url: String.t ;
      receipt_handle: String.t ;
      visibility_timeout: Integer.t }
    let make ~queue_url  ~receipt_handle  ~visibility_timeout  () =
      { queue_url; receipt_handle; visibility_timeout }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          receipt_handle =
            (Xml.required "ReceiptHandle"
               (Util.option_bind (Xml.member "ReceiptHandle" xml)
                  String.parse));
          visibility_timeout =
            (Xml.required "VisibilityTimeout"
               (Util.option_bind (Xml.member "VisibilityTimeout" xml)
                  Integer.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("VisibilityTimeout",
                   (Integer.to_query v.visibility_timeout)));
           Some
             (Query.Pair
                ("ReceiptHandle", (String.to_query v.receipt_handle)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("visibility_timeout", (Integer.to_json v.visibility_timeout));
           Some ("receipt_handle", (String.to_json v.receipt_handle));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        receipt_handle =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "receipt_handle")));
        visibility_timeout =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "visibility_timeout")))
      }
  end
module ReceiptHandleIsInvalid =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ReceiveMessageRequest =
  struct
    type t =
      {
      queue_url: String.t ;
      attribute_names: AttributeNameList.t ;
      message_attribute_names: MessageAttributeNameList.t ;
      max_number_of_messages: Integer.t option ;
      visibility_timeout: Integer.t option ;
      wait_time_seconds: Integer.t option ;
      receive_request_attempt_id: String.t option }
    let make ~queue_url  ?(attribute_names= [])  ?(message_attribute_names=
      [])  ?max_number_of_messages  ?visibility_timeout  ?wait_time_seconds 
      ?receive_request_attempt_id  () =
      {
        queue_url;
        attribute_names;
        message_attribute_names;
        max_number_of_messages;
        visibility_timeout;
        wait_time_seconds;
        receive_request_attempt_id
      }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          attribute_names = (Util.of_option [] (AttributeNameList.parse xml));
          message_attribute_names =
            (Util.of_option [] (MessageAttributeNameList.parse xml));
          max_number_of_messages =
            (Util.option_bind (Xml.member "MaxNumberOfMessages" xml)
               Integer.parse);
          visibility_timeout =
            (Util.option_bind (Xml.member "VisibilityTimeout" xml)
               Integer.parse);
          wait_time_seconds =
            (Util.option_bind (Xml.member "WaitTimeSeconds" xml)
               Integer.parse);
          receive_request_attempt_id =
            (Util.option_bind (Xml.member "ReceiveRequestAttemptId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.receive_request_attempt_id
              (fun f ->
                 Query.Pair ("ReceiveRequestAttemptId", (String.to_query f)));
           Util.option_map v.wait_time_seconds
             (fun f -> Query.Pair ("WaitTimeSeconds", (Integer.to_query f)));
           Util.option_map v.visibility_timeout
             (fun f -> Query.Pair ("VisibilityTimeout", (Integer.to_query f)));
           Util.option_map v.max_number_of_messages
             (fun f ->
                Query.Pair ("MaxNumberOfMessages", (Integer.to_query f)));
           Some
             (Query.Pair
                ("MessageAttributeNames.member",
                  (MessageAttributeNameList.to_query
                     v.message_attribute_names)));
           Some
             (Query.Pair
                ("AttributeNames.member",
                  (AttributeNameList.to_query v.attribute_names)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.receive_request_attempt_id
              (fun f -> ("receive_request_attempt_id", (String.to_json f)));
           Util.option_map v.wait_time_seconds
             (fun f -> ("wait_time_seconds", (Integer.to_json f)));
           Util.option_map v.visibility_timeout
             (fun f -> ("visibility_timeout", (Integer.to_json f)));
           Util.option_map v.max_number_of_messages
             (fun f -> ("max_number_of_messages", (Integer.to_json f)));
           Some
             ("message_attribute_names",
               (MessageAttributeNameList.to_json v.message_attribute_names));
           Some
             ("attribute_names",
               (AttributeNameList.to_json v.attribute_names));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        attribute_names =
          (AttributeNameList.of_json
             (Util.of_option_exn (Json.lookup j "attribute_names")));
        message_attribute_names =
          (MessageAttributeNameList.of_json
             (Util.of_option_exn (Json.lookup j "message_attribute_names")));
        max_number_of_messages =
          (Util.option_map (Json.lookup j "max_number_of_messages")
             Integer.of_json);
        visibility_timeout =
          (Util.option_map (Json.lookup j "visibility_timeout")
             Integer.of_json);
        wait_time_seconds =
          (Util.option_map (Json.lookup j "wait_time_seconds")
             Integer.of_json);
        receive_request_attempt_id =
          (Util.option_map (Json.lookup j "receive_request_attempt_id")
             String.of_json)
      }
  end
module SendMessageResult =
  struct
    type t =
      {
      m_d5_of_message_body: String.t option ;
      m_d5_of_message_attributes: String.t option ;
      m_d5_of_message_system_attributes: String.t option ;
      message_id: String.t option ;
      sequence_number: String.t option }
    let make ?m_d5_of_message_body  ?m_d5_of_message_attributes 
      ?m_d5_of_message_system_attributes  ?message_id  ?sequence_number  () =
      {
        m_d5_of_message_body;
        m_d5_of_message_attributes;
        m_d5_of_message_system_attributes;
        message_id;
        sequence_number
      }
    let parse xml =
      Some
        {
          m_d5_of_message_body =
            (Util.option_bind (Xml.member "MD5OfMessageBody" xml)
               String.parse);
          m_d5_of_message_attributes =
            (Util.option_bind (Xml.member "MD5OfMessageAttributes" xml)
               String.parse);
          m_d5_of_message_system_attributes =
            (Util.option_bind (Xml.member "MD5OfMessageSystemAttributes" xml)
               String.parse);
          message_id =
            (Util.option_bind (Xml.member "MessageId" xml) String.parse);
          sequence_number =
            (Util.option_bind (Xml.member "SequenceNumber" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.sequence_number
              (fun f -> Query.Pair ("SequenceNumber", (String.to_query f)));
           Util.option_map v.message_id
             (fun f -> Query.Pair ("MessageId", (String.to_query f)));
           Util.option_map v.m_d5_of_message_system_attributes
             (fun f ->
                Query.Pair
                  ("MD5OfMessageSystemAttributes", (String.to_query f)));
           Util.option_map v.m_d5_of_message_attributes
             (fun f ->
                Query.Pair ("MD5OfMessageAttributes", (String.to_query f)));
           Util.option_map v.m_d5_of_message_body
             (fun f -> Query.Pair ("MD5OfMessageBody", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.sequence_number
              (fun f -> ("sequence_number", (String.to_json f)));
           Util.option_map v.message_id
             (fun f -> ("message_id", (String.to_json f)));
           Util.option_map v.m_d5_of_message_system_attributes
             (fun f ->
                ("m_d5_of_message_system_attributes", (String.to_json f)));
           Util.option_map v.m_d5_of_message_attributes
             (fun f -> ("m_d5_of_message_attributes", (String.to_json f)));
           Util.option_map v.m_d5_of_message_body
             (fun f -> ("m_d5_of_message_body", (String.to_json f)))])
    let of_json j =
      {
        m_d5_of_message_body =
          (Util.option_map (Json.lookup j "m_d5_of_message_body")
             String.of_json);
        m_d5_of_message_attributes =
          (Util.option_map (Json.lookup j "m_d5_of_message_attributes")
             String.of_json);
        m_d5_of_message_system_attributes =
          (Util.option_map
             (Json.lookup j "m_d5_of_message_system_attributes")
             String.of_json);
        message_id =
          (Util.option_map (Json.lookup j "message_id") String.of_json);
        sequence_number =
          (Util.option_map (Json.lookup j "sequence_number") String.of_json)
      }
  end
module DeleteMessageRequest =
  struct
    type t = {
      queue_url: String.t ;
      receipt_handle: String.t }
    let make ~queue_url  ~receipt_handle  () = { queue_url; receipt_handle }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          receipt_handle =
            (Xml.required "ReceiptHandle"
               (Util.option_bind (Xml.member "ReceiptHandle" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ReceiptHandle", (String.to_query v.receipt_handle)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("receipt_handle", (String.to_json v.receipt_handle));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        receipt_handle =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "receipt_handle")))
      }
  end
module ListDeadLetterSourceQueuesRequest =
  struct
    type t =
      {
      queue_url: String.t ;
      next_token: String.t option ;
      max_results: Integer.t option }
    let make ~queue_url  ?next_token  ?max_results  () =
      { queue_url; next_token; max_results }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json)
      }
  end
module QueueDeletedRecently =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SendMessageRequest =
  struct
    type t =
      {
      queue_url: String.t ;
      message_body: String.t ;
      delay_seconds: Integer.t option ;
      message_attributes: MessageBodyAttributeMap.t option ;
      message_system_attributes: MessageBodySystemAttributeMap.t option ;
      message_deduplication_id: String.t option ;
      message_group_id: String.t option }
    let make ~queue_url  ~message_body  ?delay_seconds  ?message_attributes 
      ?message_system_attributes  ?message_deduplication_id 
      ?message_group_id  () =
      {
        queue_url;
        message_body;
        delay_seconds;
        message_attributes;
        message_system_attributes;
        message_deduplication_id;
        message_group_id
      }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          message_body =
            (Xml.required "MessageBody"
               (Util.option_bind (Xml.member "MessageBody" xml) String.parse));
          delay_seconds =
            (Util.option_bind (Xml.member "DelaySeconds" xml) Integer.parse);
          message_attributes =
            (Util.option_bind (Xml.member "MessageAttribute" xml)
               MessageBodyAttributeMap.parse);
          message_system_attributes =
            (Util.option_bind (Xml.member "MessageSystemAttribute" xml)
               MessageBodySystemAttributeMap.parse);
          message_deduplication_id =
            (Util.option_bind (Xml.member "MessageDeduplicationId" xml)
               String.parse);
          message_group_id =
            (Util.option_bind (Xml.member "MessageGroupId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message_group_id
              (fun f -> Query.Pair ("MessageGroupId", (String.to_query f)));
           Util.option_map v.message_deduplication_id
             (fun f ->
                Query.Pair ("MessageDeduplicationId", (String.to_query f)));
           Util.option_map v.message_system_attributes
             (fun f ->
                Query.Pair
                  ("MessageSystemAttribute",
                    (MessageBodySystemAttributeMap.to_query f)));
           Util.option_map v.message_attributes
             (fun f ->
                Query.Pair
                  ("MessageAttribute", (MessageBodyAttributeMap.to_query f)));
           Util.option_map v.delay_seconds
             (fun f -> Query.Pair ("DelaySeconds", (Integer.to_query f)));
           Some
             (Query.Pair ("MessageBody", (String.to_query v.message_body)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message_group_id
              (fun f -> ("message_group_id", (String.to_json f)));
           Util.option_map v.message_deduplication_id
             (fun f -> ("message_deduplication_id", (String.to_json f)));
           Util.option_map v.message_system_attributes
             (fun f ->
                ("message_system_attributes",
                  (MessageBodySystemAttributeMap.to_json f)));
           Util.option_map v.message_attributes
             (fun f ->
                ("message_attributes", (MessageBodyAttributeMap.to_json f)));
           Util.option_map v.delay_seconds
             (fun f -> ("delay_seconds", (Integer.to_json f)));
           Some ("message_body", (String.to_json v.message_body));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        message_body =
          (String.of_json (Util.of_option_exn (Json.lookup j "message_body")));
        delay_seconds =
          (Util.option_map (Json.lookup j "delay_seconds") Integer.of_json);
        message_attributes =
          (Util.option_map (Json.lookup j "message_attributes")
             MessageBodyAttributeMap.of_json);
        message_system_attributes =
          (Util.option_map (Json.lookup j "message_system_attributes")
             MessageBodySystemAttributeMap.of_json);
        message_deduplication_id =
          (Util.option_map (Json.lookup j "message_deduplication_id")
             String.of_json);
        message_group_id =
          (Util.option_map (Json.lookup j "message_group_id") String.of_json)
      }
  end
module BatchRequestTooLong =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListDeadLetterSourceQueuesResult =
  struct
    type t = {
      queue_urls: QueueUrlList.t ;
      next_token: String.t option }
    let make ~queue_urls  ?next_token  () = { queue_urls; next_token }
    let parse xml =
      Some
        {
          queue_urls = (Xml.required "queueUrls" (QueueUrlList.parse xml));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("queueUrls.member", (QueueUrlList.to_query v.queue_urls)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("queue_urls", (QueueUrlList.to_json v.queue_urls))])
    let of_json j =
      {
        queue_urls =
          (QueueUrlList.of_json
             (Util.of_option_exn (Json.lookup j "queue_urls")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module QueueDoesNotExist =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListQueuesRequest =
  struct
    type t =
      {
      queue_name_prefix: String.t option ;
      next_token: String.t option ;
      max_results: Integer.t option }
    let make ?queue_name_prefix  ?next_token  ?max_results  () =
      { queue_name_prefix; next_token; max_results }
    let parse xml =
      Some
        {
          queue_name_prefix =
            (Util.option_bind (Xml.member "QueueNamePrefix" xml) String.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.queue_name_prefix
             (fun f -> Query.Pair ("QueueNamePrefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.queue_name_prefix
             (fun f -> ("queue_name_prefix", (String.to_json f)))])
    let of_json j =
      {
        queue_name_prefix =
          (Util.option_map (Json.lookup j "queue_name_prefix") String.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json)
      }
  end
module UnsupportedOperation =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module BatchEntryIdsNotDistinct =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateQueueResult =
  struct
    type t = {
      queue_url: String.t option }
    let make ?queue_url  () = { queue_url }
    let parse xml =
      Some
        {
          queue_url =
            (Util.option_bind (Xml.member "QueueUrl" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.queue_url
              (fun f -> Query.Pair ("QueueUrl", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.queue_url
              (fun f -> ("queue_url", (String.to_json f)))])
    let of_json j =
      {
        queue_url =
          (Util.option_map (Json.lookup j "queue_url") String.of_json)
      }
  end
module TagQueueRequest =
  struct
    type t = {
      queue_url: String.t ;
      tags: TagMap.t }
    let make ~queue_url  ~tags  () = { queue_url; tags }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          tags =
            (Xml.required "Tags"
               (Util.option_bind (Xml.member "Tags" xml) TagMap.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags", (TagMap.to_query v.tags)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagMap.to_json v.tags));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        tags = (TagMap.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module InvalidAttributeName =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module OverLimit =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ReceiveMessageResult =
  struct
    type t = {
      messages: MessageList.t }
    let make ?(messages= [])  () = { messages }
    let parse xml =
      Some { messages = (Util.of_option [] (MessageList.parse xml)) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Messages.member", (MessageList.to_query v.messages)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("messages", (MessageList.to_json v.messages))])
    let of_json j =
      {
        messages =
          (MessageList.of_json
             (Util.of_option_exn (Json.lookup j "messages")))
      }
  end
module GetQueueAttributesRequest =
  struct
    type t = {
      queue_url: String.t ;
      attribute_names: AttributeNameList.t }
    let make ~queue_url  ?(attribute_names= [])  () =
      { queue_url; attribute_names }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          attribute_names = (Util.of_option [] (AttributeNameList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AttributeNames.member",
                   (AttributeNameList.to_query v.attribute_names)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("attribute_names",
                (AttributeNameList.to_json v.attribute_names));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        attribute_names =
          (AttributeNameList.of_json
             (Util.of_option_exn (Json.lookup j "attribute_names")))
      }
  end
module InvalidBatchEntryId =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteMessageBatchResult =
  struct
    type t =
      {
      successful: DeleteMessageBatchResultEntryList.t ;
      failed: BatchResultErrorEntryList.t }
    let make ~successful  ~failed  () = { successful; failed }
    let parse xml =
      Some
        {
          successful =
            (Xml.required "Successful"
               (DeleteMessageBatchResultEntryList.parse xml));
          failed =
            (Xml.required "Failed" (BatchResultErrorEntryList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Failed.member",
                   (BatchResultErrorEntryList.to_query v.failed)));
           Some
             (Query.Pair
                ("Successful.member",
                  (DeleteMessageBatchResultEntryList.to_query v.successful)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("failed", (BatchResultErrorEntryList.to_json v.failed));
           Some
             ("successful",
               (DeleteMessageBatchResultEntryList.to_json v.successful))])
    let of_json j =
      {
        successful =
          (DeleteMessageBatchResultEntryList.of_json
             (Util.of_option_exn (Json.lookup j "successful")));
        failed =
          (BatchResultErrorEntryList.of_json
             (Util.of_option_exn (Json.lookup j "failed")))
      }
  end
module ListQueueTagsResult =
  struct
    type t = {
      tags: TagMap.t option }
    let make ?tags  () = { tags }
    let parse xml =
      Some { tags = (Util.option_bind (Xml.member "Tag" xml) TagMap.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.tags
              (fun f -> Query.Pair ("Tag", (TagMap.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.tags (fun f -> ("tags", (TagMap.to_json f)))])
    let of_json j =
      { tags = (Util.option_map (Json.lookup j "tags") TagMap.of_json) }
  end
module UntagQueueRequest =
  struct
    type t = {
      queue_url: String.t ;
      tag_keys: TagKeyList.t }
    let make ~queue_url  ~tag_keys  () = { queue_url; tag_keys }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          tag_keys = (Xml.required "TagKeys" (TagKeyList.parse xml))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TagKeys.member", (TagKeyList.to_query v.tag_keys)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tag_keys", (TagKeyList.to_json v.tag_keys));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        tag_keys =
          (TagKeyList.of_json (Util.of_option_exn (Json.lookup j "tag_keys")))
      }
  end
module TooManyEntriesInBatchRequest =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListQueuesResult =
  struct
    type t = {
      queue_urls: QueueUrlList.t ;
      next_token: String.t option }
    let make ?(queue_urls= [])  ?next_token  () = { queue_urls; next_token }
    let parse xml =
      Some
        {
          queue_urls = (Util.of_option [] (QueueUrlList.parse xml));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("QueueUrls.member", (QueueUrlList.to_query v.queue_urls)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("queue_urls", (QueueUrlList.to_json v.queue_urls))])
    let of_json j =
      {
        queue_urls =
          (QueueUrlList.of_json
             (Util.of_option_exn (Json.lookup j "queue_urls")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module CreateQueueRequest =
  struct
    type t =
      {
      queue_name: String.t ;
      attributes: QueueAttributeMap.t option ;
      tags: TagMap.t option }
    let make ~queue_name  ?attributes  ?tags  () =
      { queue_name; attributes; tags }
    let parse xml =
      Some
        {
          queue_name =
            (Xml.required "QueueName"
               (Util.option_bind (Xml.member "QueueName" xml) String.parse));
          attributes =
            (Util.option_bind (Xml.member "Attribute" xml)
               QueueAttributeMap.parse);
          tags = (Util.option_bind (Xml.member "Tag" xml) TagMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.tags
              (fun f -> Query.Pair ("Tag", (TagMap.to_query f)));
           Util.option_map v.attributes
             (fun f ->
                Query.Pair ("Attribute", (QueueAttributeMap.to_query f)));
           Some (Query.Pair ("QueueName", (String.to_query v.queue_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.tags (fun f -> ("tags", (TagMap.to_json f)));
           Util.option_map v.attributes
             (fun f -> ("attributes", (QueueAttributeMap.to_json f)));
           Some ("queue_name", (String.to_json v.queue_name))])
    let of_json j =
      {
        queue_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_name")));
        attributes =
          (Util.option_map (Json.lookup j "attributes")
             QueueAttributeMap.of_json);
        tags = (Util.option_map (Json.lookup j "tags") TagMap.of_json)
      }
  end
module InvalidMessageContents =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SetQueueAttributesRequest =
  struct
    type t = {
      queue_url: String.t ;
      attributes: QueueAttributeMap.t }
    let make ~queue_url  ~attributes  () = { queue_url; attributes }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse));
          attributes =
            (Xml.required "Attribute"
               (Util.option_bind (Xml.member "Attribute" xml)
                  QueueAttributeMap.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Attribute", (QueueAttributeMap.to_query v.attributes)));
           Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("attributes", (QueueAttributeMap.to_json v.attributes));
           Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")));
        attributes =
          (QueueAttributeMap.of_json
             (Util.of_option_exn (Json.lookup j "attributes")))
      }
  end
module PurgeQueueRequest =
  struct
    type t = {
      queue_url: String.t }
    let make ~queue_url  () = { queue_url }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")))
      }
  end
module ListQueueTagsRequest =
  struct
    type t = {
      queue_url: String.t }
    let make ~queue_url  () = { queue_url }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")))
      }
  end
module QueueNameExists =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GetQueueUrlResult =
  struct
    type t = {
      queue_url: String.t }
    let make ~queue_url  () = { queue_url }
    let parse xml =
      Some
        {
          queue_url =
            (Xml.required "QueueUrl"
               (Util.option_bind (Xml.member "QueueUrl" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("QueueUrl", (String.to_query v.queue_url)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("queue_url", (String.to_json v.queue_url))])
    let of_json j =
      {
        queue_url =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_url")))
      }
  end