open Aws.BaseTypes

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
