type t = { key_pairs : KeyPairList.t }

let make ?(key_pairs = []) () = { key_pairs }

let parse xml =
  Some
    { key_pairs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "keySet" xml) KeyPairList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("KeySet", KeyPairList.to_query v.key_pairs)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("keySet", KeyPairList.to_json v.key_pairs) ])

let of_json j =
  { key_pairs = KeyPairList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "keySet"))
  }
