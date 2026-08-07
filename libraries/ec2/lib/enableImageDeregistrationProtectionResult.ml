open Aws.BaseTypes

type t = { return : String.t option }

let make ?return () = { return }

let parse xml =
  Some { return = Aws.Util.option_bind (Aws.Xml.member "return" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return (fun f ->
             Aws.Query.Pair ("Return", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return (fun f -> "return", String.to_json f) ])

let of_json j =
  { return = Aws.Util.option_map (Aws.Json.lookup j "return") String.of_json }
