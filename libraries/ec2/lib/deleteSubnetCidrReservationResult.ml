type t = { deleted_subnet_cidr_reservation : SubnetCidrReservation.t option }

let make ?deleted_subnet_cidr_reservation () = { deleted_subnet_cidr_reservation }

let parse xml =
  Some
    { deleted_subnet_cidr_reservation =
        Aws.Util.option_bind
          (Aws.Xml.member "deletedSubnetCidrReservation" xml)
          SubnetCidrReservation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.deleted_subnet_cidr_reservation (fun f ->
             Aws.Query.Pair
               ("DeletedSubnetCidrReservation", SubnetCidrReservation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.deleted_subnet_cidr_reservation (fun f ->
             "deletedSubnetCidrReservation", SubnetCidrReservation.to_json f)
       ])

let of_json j =
  { deleted_subnet_cidr_reservation =
      Aws.Util.option_map
        (Aws.Json.lookup j "deletedSubnetCidrReservation")
        SubnetCidrReservation.of_json
  }
