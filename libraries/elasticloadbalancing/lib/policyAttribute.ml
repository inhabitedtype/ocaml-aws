open Aws.BaseTypes

type t =
  { attribute_name : String.t option
  ; attribute_value : String.t option
  }

let make ?attribute_name ?attribute_value () = { attribute_name; attribute_value }

let parse xml =
  Some
    { attribute_name =
        Aws.Util.option_bind (Aws.Xml.member "AttributeName" xml) String.parse
    ; attribute_value =
        Aws.Util.option_bind (Aws.Xml.member "AttributeValue" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attribute_value (fun f ->
             Aws.Query.Pair ("AttributeValue", String.to_query f))
       ; Aws.Util.option_map v.attribute_name (fun f ->
             Aws.Query.Pair ("AttributeName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attribute_value (fun f ->
             "AttributeValue", String.to_json f)
       ; Aws.Util.option_map v.attribute_name (fun f -> "AttributeName", String.to_json f)
       ])

let of_json j =
  { attribute_name =
      Aws.Util.option_map (Aws.Json.lookup j "AttributeName") String.of_json
  ; attribute_value =
      Aws.Util.option_map (Aws.Json.lookup j "AttributeValue") String.of_json
  }
