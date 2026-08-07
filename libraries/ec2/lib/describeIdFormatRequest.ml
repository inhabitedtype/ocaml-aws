open Aws.BaseTypes

type t = { resource : String.t option }

let make ?resource () = { resource }

let parse xml =
  Some { resource = Aws.Util.option_bind (Aws.Xml.member "Resource" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource (fun f ->
             Aws.Query.Pair ("Resource", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource (fun f -> "Resource", String.to_json f) ])

let of_json j =
  { resource = Aws.Util.option_map (Aws.Json.lookup j "Resource") String.of_json }
