open Aws.BaseTypes

type t = { publisher_id : String.t option }

let make ?publisher_id () = { publisher_id }

let parse xml =
  Some
    { publisher_id = Aws.Util.option_bind (Aws.Xml.member "PublisherId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.publisher_id (fun f ->
             Aws.Query.Pair ("PublisherId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.publisher_id (fun f -> "PublisherId", String.to_json f) ])

let of_json j =
  { publisher_id = Aws.Util.option_map (Aws.Json.lookup j "PublisherId") String.of_json }
