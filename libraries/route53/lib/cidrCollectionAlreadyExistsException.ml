open Aws.BaseTypes

type t = { message : String.t option }

let make ?message () = { message }

let parse xml =
  Some { message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("Message", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f -> "Message", String.to_json f) ])

let of_json j =
  { message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json }
