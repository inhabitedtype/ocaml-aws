type t = { ipam_pool : IpamPool.t option }

let make ?ipam_pool () = { ipam_pool }

let parse xml =
  Some { ipam_pool = Aws.Util.option_bind (Aws.Xml.member "ipamPool" xml) IpamPool.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_pool (fun f ->
             Aws.Query.Pair ("IpamPool", IpamPool.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_pool (fun f -> "ipamPool", IpamPool.to_json f) ])

let of_json j =
  { ipam_pool = Aws.Util.option_map (Aws.Json.lookup j "ipamPool") IpamPool.of_json }
