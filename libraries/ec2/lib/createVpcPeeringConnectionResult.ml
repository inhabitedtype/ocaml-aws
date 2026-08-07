type t = { vpc_peering_connection : VpcPeeringConnection.t option }

let make ?vpc_peering_connection () = { vpc_peering_connection }

let parse xml =
  Some
    { vpc_peering_connection =
        Aws.Util.option_bind
          (Aws.Xml.member "vpcPeeringConnection" xml)
          VpcPeeringConnection.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_peering_connection (fun f ->
             Aws.Query.Pair ("VpcPeeringConnection", VpcPeeringConnection.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_peering_connection (fun f ->
             "vpcPeeringConnection", VpcPeeringConnection.to_json f)
       ])

let of_json j =
  { vpc_peering_connection =
      Aws.Util.option_map
        (Aws.Json.lookup j "vpcPeeringConnection")
        VpcPeeringConnection.of_json
  }
