open Aws.BaseTypes

type t = { size_in_mi_b : Integer.t option }

let make ?size_in_mi_b () = { size_in_mi_b }

let parse xml =
  Some
    { size_in_mi_b = Aws.Util.option_bind (Aws.Xml.member "sizeInMiB" xml) Integer.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.size_in_mi_b (fun f ->
             Aws.Query.Pair ("SizeInMiB", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.size_in_mi_b (fun f -> "sizeInMiB", Integer.to_json f) ])

let of_json j =
  { size_in_mi_b = Aws.Util.option_map (Aws.Json.lookup j "sizeInMiB") Integer.of_json }
