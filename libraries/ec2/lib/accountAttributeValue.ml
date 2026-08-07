open Aws.BaseTypes

type t = { attribute_value : String.t option }

let make ?attribute_value () = { attribute_value }

let parse xml =
  Some
    { attribute_value =
        Aws.Util.option_bind (Aws.Xml.member "attributeValue" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attribute_value (fun f ->
             Aws.Query.Pair ("AttributeValue", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attribute_value (fun f ->
             "attributeValue", String.to_json f)
       ])

let of_json j =
  { attribute_value =
      Aws.Util.option_map (Aws.Json.lookup j "attributeValue") String.of_json
  }
