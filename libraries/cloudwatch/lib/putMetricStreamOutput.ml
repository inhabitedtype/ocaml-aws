open Aws.BaseTypes

type t = { arn : String.t option }

let make ?arn () = { arn }

let parse xml =
  Some { arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f) ])

let of_json j = { arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json }
