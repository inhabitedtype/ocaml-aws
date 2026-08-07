open Aws.BaseTypes

type t =
  { attribute_name : String.t option
  ; attribute_values : AccountAttributeValueList.t
  }

let make ?attribute_name ?(attribute_values = []) () =
  { attribute_name; attribute_values }

let parse xml =
  Some
    { attribute_name =
        Aws.Util.option_bind (Aws.Xml.member "attributeName" xml) String.parse
    ; attribute_values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "attributeValueSet" xml)
             AccountAttributeValueList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AttributeValueSet", AccountAttributeValueList.to_query v.attribute_values))
       ; Aws.Util.option_map v.attribute_name (fun f ->
             Aws.Query.Pair ("AttributeName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("attributeValueSet", AccountAttributeValueList.to_json v.attribute_values)
       ; Aws.Util.option_map v.attribute_name (fun f -> "attributeName", String.to_json f)
       ])

let of_json j =
  { attribute_name =
      Aws.Util.option_map (Aws.Json.lookup j "attributeName") String.of_json
  ; attribute_values =
      AccountAttributeValueList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "attributeValueSet"))
  }
