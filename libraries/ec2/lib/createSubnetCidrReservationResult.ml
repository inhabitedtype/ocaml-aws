type t = { subnet_cidr_reservation : SubnetCidrReservation.t option }

let make ?subnet_cidr_reservation () = { subnet_cidr_reservation }

let parse xml =
  Some
    { subnet_cidr_reservation =
        Aws.Util.option_bind
          (Aws.Xml.member "subnetCidrReservation" xml)
          SubnetCidrReservation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_cidr_reservation (fun f ->
             Aws.Query.Pair ("SubnetCidrReservation", SubnetCidrReservation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_cidr_reservation (fun f ->
             "subnetCidrReservation", SubnetCidrReservation.to_json f)
       ])

let of_json j =
  { subnet_cidr_reservation =
      Aws.Util.option_map
        (Aws.Json.lookup j "subnetCidrReservation")
        SubnetCidrReservation.of_json
  }
