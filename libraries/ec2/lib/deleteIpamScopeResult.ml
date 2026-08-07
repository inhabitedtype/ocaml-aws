type t = { ipam_scope : IpamScope.t option }

let make ?ipam_scope () = { ipam_scope }

let parse xml =
  Some
    { ipam_scope = Aws.Util.option_bind (Aws.Xml.member "ipamScope" xml) IpamScope.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_scope (fun f ->
             Aws.Query.Pair ("IpamScope", IpamScope.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_scope (fun f -> "ipamScope", IpamScope.to_json f) ])

let of_json j =
  { ipam_scope = Aws.Util.option_map (Aws.Json.lookup j "ipamScope") IpamScope.of_json }
