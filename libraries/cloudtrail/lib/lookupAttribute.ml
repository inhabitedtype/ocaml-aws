open Aws.BaseTypes

type t =
  { attribute_key : LookupAttributeKey.t
  ; attribute_value : String.t
  }

let make ~attribute_key ~attribute_value () = { attribute_key; attribute_value }

let parse xml =
  Some
    { attribute_key =
        Aws.Xml.required
          "AttributeKey"
          (Aws.Util.option_bind
             (Aws.Xml.member "AttributeKey" xml)
             LookupAttributeKey.parse)
    ; attribute_value =
        Aws.Xml.required
          "AttributeValue"
          (Aws.Util.option_bind (Aws.Xml.member "AttributeValue" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("AttributeValue", String.to_query v.attribute_value))
       ; Some
           (Aws.Query.Pair ("AttributeKey", LookupAttributeKey.to_query v.attribute_key))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AttributeValue", String.to_json v.attribute_value)
       ; Some ("AttributeKey", LookupAttributeKey.to_json v.attribute_key)
       ])

let of_json j =
  { attribute_key =
      LookupAttributeKey.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeKey"))
  ; attribute_value =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeValue"))
  }
