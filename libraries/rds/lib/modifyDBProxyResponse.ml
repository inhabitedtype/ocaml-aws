type t = { d_b_proxy : DBProxy.t option }

let make ?d_b_proxy () = { d_b_proxy }

let parse xml =
  Some { d_b_proxy = Aws.Util.option_bind (Aws.Xml.member "DBProxy" xml) DBProxy.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_proxy (fun f ->
             Aws.Query.Pair ("DBProxy", DBProxy.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_proxy (fun f -> "DBProxy", DBProxy.to_json f) ])

let of_json j =
  { d_b_proxy = Aws.Util.option_map (Aws.Json.lookup j "DBProxy") DBProxy.of_json }
