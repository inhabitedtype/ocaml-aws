open Aws.BaseTypes

type t = { custom_key_store_id : String.t }

let make ~custom_key_store_id () = { custom_key_store_id }

let parse xml =
  Some
    { custom_key_store_id =
        Aws.Xml.required
          "CustomKeyStoreId"
          (Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("CustomKeyStoreId", String.to_query v.custom_key_store_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CustomKeyStoreId", String.to_json v.custom_key_store_id) ])

let of_json j =
  { custom_key_store_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomKeyStoreId"))
  }
