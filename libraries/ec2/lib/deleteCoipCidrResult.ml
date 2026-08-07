type t = { coip_cidr : CoipCidr.t option }

let make ?coip_cidr () = { coip_cidr }

let parse xml =
  Some { coip_cidr = Aws.Util.option_bind (Aws.Xml.member "coipCidr" xml) CoipCidr.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.coip_cidr (fun f ->
             Aws.Query.Pair ("CoipCidr", CoipCidr.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.coip_cidr (fun f -> "coipCidr", CoipCidr.to_json f) ])

let of_json j =
  { coip_cidr = Aws.Util.option_map (Aws.Json.lookup j "coipCidr") CoipCidr.of_json }
