open Aws.BaseTypes

type t = { d_n_s_name : String.t option }

let make ?d_n_s_name () = { d_n_s_name }

let parse xml =
  Some { d_n_s_name = Aws.Util.option_bind (Aws.Xml.member "DNSName" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_n_s_name (fun f ->
             Aws.Query.Pair ("DNSName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_n_s_name (fun f -> "DNSName", String.to_json f) ])

let of_json j =
  { d_n_s_name = Aws.Util.option_map (Aws.Json.lookup j "DNSName") String.of_json }
