type t = { byoip_cidr : ByoipCidr.t option }

let make ?byoip_cidr () = { byoip_cidr }

let parse xml =
  Some
    { byoip_cidr = Aws.Util.option_bind (Aws.Xml.member "byoipCidr" xml) ByoipCidr.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.byoip_cidr (fun f ->
             Aws.Query.Pair ("ByoipCidr", ByoipCidr.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.byoip_cidr (fun f -> "byoipCidr", ByoipCidr.to_json f) ])

let of_json j =
  { byoip_cidr = Aws.Util.option_map (Aws.Json.lookup j "byoipCidr") ByoipCidr.of_json }
