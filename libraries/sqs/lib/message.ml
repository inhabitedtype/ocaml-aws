open Aws.BaseTypes

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
          (Aws.Xml.member "Attributes" xml)
          MessageSystemAttributeMap.parse
    ; m_d5_of_message_attributes =
        Aws.Util.option_bind (Aws.Xml.member "MD5OfMessageAttributes" xml) String.parse
    ; message_attributes =
        Aws.Util.option_bind
          (Aws.Xml.member "MessageAttributes" xml)
          MessageBodyAttributeMap.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message_attributes (fun f ->
             Aws.Query.Pair ("MessageAttributes", MessageBodyAttributeMap.to_query f))
       ; Aws.Util.option_map v.m_d5_of_message_attributes (fun f ->
             Aws.Query.Pair ("MD5OfMessageAttributes", String.to_query f))
       ; Aws.Util.option_map v.attributes (fun f ->
             Aws.Query.Pair ("Attributes", MessageSystemAttributeMap.to_query f))
       ; Aws.Util.option_map v.body (fun f -> Aws.Query.Pair ("Body", String.to_query f))
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
             "MessageAttributes", MessageBodyAttributeMap.to_json f)
       ; Aws.Util.option_map v.m_d5_of_message_attributes (fun f ->
             "MD5OfMessageAttributes", String.to_json f)
       ; Aws.Util.option_map v.attributes (fun f ->
             "Attributes", MessageSystemAttributeMap.to_json f)
       ; Aws.Util.option_map v.body (fun f -> "Body", String.to_json f)
       ; Aws.Util.option_map v.m_d5_of_body (fun f -> "MD5OfBody", String.to_json f)
       ; Aws.Util.option_map v.receipt_handle (fun f -> "ReceiptHandle", String.to_json f)
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
        (Aws.Json.lookup j "Attributes")
        MessageSystemAttributeMap.of_json
  ; m_d5_of_message_attributes =
      Aws.Util.option_map (Aws.Json.lookup j "MD5OfMessageAttributes") String.of_json
  ; message_attributes =
      Aws.Util.option_map
        (Aws.Json.lookup j "MessageAttributes")
        MessageBodyAttributeMap.of_json
  }
