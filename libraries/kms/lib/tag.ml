open Aws.BaseTypes

type t =
  { tag_key : String.t
  ; tag_value : String.t
  }

let make ~tag_key ~tag_value () = { tag_key; tag_value }

let parse xml =
  Some
    { tag_key =
        Aws.Xml.required
          "TagKey"
          (Aws.Util.option_bind (Aws.Xml.member "TagKey" xml) String.parse)
    ; tag_value =
        Aws.Xml.required
          "TagValue"
          (Aws.Util.option_bind (Aws.Xml.member "TagValue" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagValue", String.to_query v.tag_value))
       ; Some (Aws.Query.Pair ("TagKey", String.to_query v.tag_key))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagValue", String.to_json v.tag_value)
       ; Some ("TagKey", String.to_json v.tag_key)
       ])

let of_json j =
  { tag_key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagKey"))
  ; tag_value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagValue"))
  }
