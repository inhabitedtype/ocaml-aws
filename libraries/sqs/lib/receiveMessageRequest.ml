open Aws.BaseTypes

type t =
  { queue_url : String.t
  ; attribute_names : AttributeNameList.t
  ; message_system_attribute_names : MessageSystemAttributeList.t
  ; message_attribute_names : MessageAttributeNameList.t
  ; max_number_of_messages : Integer.t option
  ; visibility_timeout : Integer.t option
  ; wait_time_seconds : Integer.t option
  ; receive_request_attempt_id : String.t option
  }

let make
    ~queue_url
    ?(attribute_names = [])
    ?(message_system_attribute_names = [])
    ?(message_attribute_names = [])
    ?max_number_of_messages
    ?visibility_timeout
    ?wait_time_seconds
    ?receive_request_attempt_id
    () =
  { queue_url
  ; attribute_names
  ; message_system_attribute_names
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
    ; message_system_attribute_names =
        Aws.Util.of_option [] (MessageSystemAttributeList.parse xml)
    ; message_attribute_names = Aws.Util.of_option [] (MessageAttributeNameList.parse xml)
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
              ( "MessageSystemAttributeNames.member"
              , MessageSystemAttributeList.to_query v.message_system_attribute_names ))
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
       ; Some
           ( "MessageSystemAttributeNames"
           , MessageSystemAttributeList.to_json v.message_system_attribute_names )
       ; Some ("AttributeNames", AttributeNameList.to_json v.attribute_names)
       ; Some ("QueueUrl", String.to_json v.queue_url)
       ])

let of_json j =
  { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
  ; attribute_names =
      AttributeNameList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeNames"))
  ; message_system_attribute_names =
      MessageSystemAttributeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MessageSystemAttributeNames"))
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
