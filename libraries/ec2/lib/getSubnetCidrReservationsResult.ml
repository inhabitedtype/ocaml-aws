open Aws.BaseTypes

type t =
  { subnet_ipv4_cidr_reservations : SubnetCidrReservationList.t
  ; subnet_ipv6_cidr_reservations : SubnetCidrReservationList.t
  ; next_token : String.t option
  }

let make
    ?(subnet_ipv4_cidr_reservations = [])
    ?(subnet_ipv6_cidr_reservations = [])
    ?next_token
    () =
  { subnet_ipv4_cidr_reservations; subnet_ipv6_cidr_reservations; next_token }

let parse xml =
  Some
    { subnet_ipv4_cidr_reservations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "subnetIpv4CidrReservationSet" xml)
             SubnetCidrReservationList.parse)
    ; subnet_ipv6_cidr_reservations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "subnetIpv6CidrReservationSet" xml)
             SubnetCidrReservationList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SubnetIpv6CidrReservationSet"
              , SubnetCidrReservationList.to_query v.subnet_ipv6_cidr_reservations ))
       ; Some
           (Aws.Query.Pair
              ( "SubnetIpv4CidrReservationSet"
              , SubnetCidrReservationList.to_query v.subnet_ipv4_cidr_reservations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "subnetIpv6CidrReservationSet"
           , SubnetCidrReservationList.to_json v.subnet_ipv6_cidr_reservations )
       ; Some
           ( "subnetIpv4CidrReservationSet"
           , SubnetCidrReservationList.to_json v.subnet_ipv4_cidr_reservations )
       ])

let of_json j =
  { subnet_ipv4_cidr_reservations =
      SubnetCidrReservationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetIpv4CidrReservationSet"))
  ; subnet_ipv6_cidr_reservations =
      SubnetCidrReservationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetIpv6CidrReservationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
