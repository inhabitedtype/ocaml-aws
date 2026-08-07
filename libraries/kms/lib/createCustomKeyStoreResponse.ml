open Aws.BaseTypes

type t = { custom_key_store_id : String.t option }

let make ?custom_key_store_id () = { custom_key_store_id }

let parse xml =
  Some
    { custom_key_store_id =
        Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.custom_key_store_id (fun f ->
             Aws.Query.Pair ("CustomKeyStoreId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.custom_key_store_id (fun f ->
             "CustomKeyStoreId", String.to_json f)
       ])

let of_json j =
  { custom_key_store_id =
      Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreId") String.of_json
  }
