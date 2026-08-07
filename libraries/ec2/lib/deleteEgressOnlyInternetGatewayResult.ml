open Aws.BaseTypes

type t = { return_code : Boolean.t option }

let make ?return_code () = { return_code }

let parse xml =
  Some
    { return_code = Aws.Util.option_bind (Aws.Xml.member "returnCode" xml) Boolean.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_code (fun f ->
             Aws.Query.Pair ("ReturnCode", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_code (fun f -> "returnCode", Boolean.to_json f) ])

let of_json j =
  { return_code = Aws.Util.option_map (Aws.Json.lookup j "returnCode") Boolean.of_json }
