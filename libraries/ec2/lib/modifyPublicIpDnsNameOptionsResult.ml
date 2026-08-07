open Aws.BaseTypes

type t = { successful : Boolean.t option }

let make ?successful () = { successful }

let parse xml =
  Some
    { successful = Aws.Util.option_bind (Aws.Xml.member "successful" xml) Boolean.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.successful (fun f ->
             Aws.Query.Pair ("Successful", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.successful (fun f -> "successful", Boolean.to_json f) ])

let of_json j =
  { successful = Aws.Util.option_map (Aws.Json.lookup j "successful") Boolean.of_json }
