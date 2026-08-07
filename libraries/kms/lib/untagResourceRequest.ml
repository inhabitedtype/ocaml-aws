open Aws.BaseTypes

type t =
  { key_id : String.t
  ; tag_keys : TagKeyList.t
  }

let make ~key_id ~tag_keys () = { key_id; tag_keys }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; tag_keys =
        Aws.Xml.required
          "TagKeys"
          (Aws.Util.option_bind (Aws.Xml.member "TagKeys" xml) TagKeyList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagKeys.member", TagKeyList.to_query v.tag_keys))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagKeys", TagKeyList.to_json v.tag_keys)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; tag_keys = TagKeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagKeys"))
  }
