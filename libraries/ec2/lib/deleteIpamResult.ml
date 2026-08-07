type t = { ipam : Ipam.t option }

let make ?ipam () = { ipam }

let parse xml =
  Some { ipam = Aws.Util.option_bind (Aws.Xml.member "ipam" xml) Ipam.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam (fun f -> Aws.Query.Pair ("Ipam", Ipam.to_query f)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam (fun f -> "ipam", Ipam.to_json f) ])

let of_json j = { ipam = Aws.Util.option_map (Aws.Json.lookup j "ipam") Ipam.of_json }
