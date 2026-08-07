open Aws.BaseTypes

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
