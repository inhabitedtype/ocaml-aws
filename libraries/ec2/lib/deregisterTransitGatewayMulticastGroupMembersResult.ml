type t =
  { deregistered_multicast_group_members :
      TransitGatewayMulticastDeregisteredGroupMembers.t option
  }

let make ?deregistered_multicast_group_members () =
  { deregistered_multicast_group_members }

let parse xml =
  Some
    { deregistered_multicast_group_members =
        Aws.Util.option_bind
          (Aws.Xml.member "deregisteredMulticastGroupMembers" xml)
          TransitGatewayMulticastDeregisteredGroupMembers.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.deregistered_multicast_group_members (fun f ->
             Aws.Query.Pair
               ( "DeregisteredMulticastGroupMembers"
               , TransitGatewayMulticastDeregisteredGroupMembers.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.deregistered_multicast_group_members (fun f ->
             ( "deregisteredMulticastGroupMembers"
             , TransitGatewayMulticastDeregisteredGroupMembers.to_json f ))
       ])

let of_json j =
  { deregistered_multicast_group_members =
      Aws.Util.option_map
        (Aws.Json.lookup j "deregisteredMulticastGroupMembers")
        TransitGatewayMulticastDeregisteredGroupMembers.of_json
  }
