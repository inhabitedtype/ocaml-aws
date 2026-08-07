open Aws.BaseTypes

type t = { value : String.t }

let make ~value () = { value }

let parse xml =
  Some
    { value =
        Aws.Xml.required
          "Value"
          (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Value", String.to_query v.value)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Value", String.to_json v.value) ])

let of_json j =
  { value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value")) }
