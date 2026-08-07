open Aws.BaseTypes

type t = { url : String.t option }

let make ?url () = { url }

let parse xml =
  Some { url = Aws.Util.option_bind (Aws.Xml.member "Url" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.url (fun f -> Aws.Query.Pair ("Url", String.to_query f)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.url (fun f -> "Url", String.to_json f) ])

let of_json j = { url = Aws.Util.option_map (Aws.Json.lookup j "Url") String.of_json }
