type t =
  { deregistered_multicast_group_sources :
      TransitGatewayMulticastDeregisteredGroupSources.t option
  }

let make ?deregistered_multicast_group_sources () =
  { deregistered_multicast_group_sources }

let parse xml =
  Some
    { deregistered_multicast_group_sources =
        Aws.Util.option_bind
          (Aws.Xml.member "deregisteredMulticastGroupSources" xml)
          TransitGatewayMulticastDeregisteredGroupSources.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.deregistered_multicast_group_sources (fun f ->
             Aws.Query.Pair
               ( "DeregisteredMulticastGroupSources"
               , TransitGatewayMulticastDeregisteredGroupSources.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.deregistered_multicast_group_sources (fun f ->
             ( "deregisteredMulticastGroupSources"
             , TransitGatewayMulticastDeregisteredGroupSources.to_json f ))
       ])

let of_json j =
  { deregistered_multicast_group_sources =
      Aws.Util.option_map
        (Aws.Json.lookup j "deregisteredMulticastGroupSources")
        TransitGatewayMulticastDeregisteredGroupSources.of_json
  }
