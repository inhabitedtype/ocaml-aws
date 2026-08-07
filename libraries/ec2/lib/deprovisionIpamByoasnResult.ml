type t = { byoasn : Byoasn.t option }

let make ?byoasn () = { byoasn }

let parse xml =
  Some { byoasn = Aws.Util.option_bind (Aws.Xml.member "byoasn" xml) Byoasn.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.byoasn (fun f ->
             Aws.Query.Pair ("Byoasn", Byoasn.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.byoasn (fun f -> "byoasn", Byoasn.to_json f) ])

let of_json j =
  { byoasn = Aws.Util.option_map (Aws.Json.lookup j "byoasn") Byoasn.of_json }
