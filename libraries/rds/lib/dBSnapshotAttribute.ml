open Aws.BaseTypes

type t =
  { attribute_name : String.t option
  ; attribute_values : AttributeValueList.t
  }

let make ?attribute_name ?(attribute_values = []) () =
  { attribute_name; attribute_values }

let parse xml =
  Some
    { attribute_name =
        Aws.Util.option_bind (Aws.Xml.member "AttributeName" xml) String.parse
    ; attribute_values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AttributeValues" xml)
             AttributeValueList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AttributeValues.member", AttributeValueList.to_query v.attribute_values))
       ; Aws.Util.option_map v.attribute_name (fun f ->
             Aws.Query.Pair ("AttributeName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AttributeValues", AttributeValueList.to_json v.attribute_values)
       ; Aws.Util.option_map v.attribute_name (fun f -> "AttributeName", String.to_json f)
       ])

let of_json j =
  { attribute_name =
      Aws.Util.option_map (Aws.Json.lookup j "AttributeName") String.of_json
  ; attribute_values =
      AttributeValueList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeValues"))
  }
