type t =
  { registered_multicast_group_members :
      TransitGatewayMulticastRegisteredGroupMembers.t option
  }

let make ?registered_multicast_group_members () = { registered_multicast_group_members }

let parse xml =
  Some
    { registered_multicast_group_members =
        Aws.Util.option_bind
          (Aws.Xml.member "registeredMulticastGroupMembers" xml)
          TransitGatewayMulticastRegisteredGroupMembers.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.registered_multicast_group_members (fun f ->
             Aws.Query.Pair
               ( "RegisteredMulticastGroupMembers"
               , TransitGatewayMulticastRegisteredGroupMembers.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.registered_multicast_group_members (fun f ->
             ( "registeredMulticastGroupMembers"
             , TransitGatewayMulticastRegisteredGroupMembers.to_json f ))
       ])

let of_json j =
  { registered_multicast_group_members =
      Aws.Util.option_map
        (Aws.Json.lookup j "registeredMulticastGroupMembers")
        TransitGatewayMulticastRegisteredGroupMembers.of_json
  }
