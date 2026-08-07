open Aws.BaseTypes

type t =
  { number_of_bytes : Integer.t option
  ; custom_key_store_id : String.t option
  ; recipient : RecipientInfo.t option
  }

let make ?number_of_bytes ?custom_key_store_id ?recipient () =
  { number_of_bytes; custom_key_store_id; recipient }

let parse xml =
  Some
    { number_of_bytes =
        Aws.Util.option_bind (Aws.Xml.member "NumberOfBytes" xml) Integer.parse
    ; custom_key_store_id =
        Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse
    ; recipient =
        Aws.Util.option_bind (Aws.Xml.member "Recipient" xml) RecipientInfo.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recipient (fun f ->
             Aws.Query.Pair ("Recipient", RecipientInfo.to_query f))
       ; Aws.Util.option_map v.custom_key_store_id (fun f ->
             Aws.Query.Pair ("CustomKeyStoreId", String.to_query f))
       ; Aws.Util.option_map v.number_of_bytes (fun f ->
             Aws.Query.Pair ("NumberOfBytes", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recipient (fun f -> "Recipient", RecipientInfo.to_json f)
       ; Aws.Util.option_map v.custom_key_store_id (fun f ->
             "CustomKeyStoreId", String.to_json f)
       ; Aws.Util.option_map v.number_of_bytes (fun f ->
             "NumberOfBytes", Integer.to_json f)
       ])

let of_json j =
  { number_of_bytes =
      Aws.Util.option_map (Aws.Json.lookup j "NumberOfBytes") Integer.of_json
  ; custom_key_store_id =
      Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreId") String.of_json
  ; recipient = Aws.Util.option_map (Aws.Json.lookup j "Recipient") RecipientInfo.of_json
  }
