open Aws.BaseTypes

type t = { return_value : Boolean.t option }

let make ?return_value () = { return_value }

let parse xml =
  Some
    { return_value = Aws.Util.option_bind (Aws.Xml.member "returnValue" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_value (fun f ->
             Aws.Query.Pair ("ReturnValue", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_value (fun f -> "returnValue", Boolean.to_json f) ])

let of_json j =
  { return_value = Aws.Util.option_map (Aws.Json.lookup j "returnValue") Boolean.of_json }
