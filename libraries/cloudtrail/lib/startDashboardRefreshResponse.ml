open Aws.BaseTypes

type t = { refresh_id : String.t option }

let make ?refresh_id () = { refresh_id }

let parse xml =
  Some { refresh_id = Aws.Util.option_bind (Aws.Xml.member "RefreshId" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.refresh_id (fun f ->
             Aws.Query.Pair ("RefreshId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.refresh_id (fun f -> "RefreshId", String.to_json f) ])

let of_json j =
  { refresh_id = Aws.Util.option_map (Aws.Json.lookup j "RefreshId") String.of_json }
