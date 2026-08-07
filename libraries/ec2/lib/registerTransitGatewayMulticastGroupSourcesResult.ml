type t =
  { registered_multicast_group_sources :
      TransitGatewayMulticastRegisteredGroupSources.t option
  }

let make ?registered_multicast_group_sources () = { registered_multicast_group_sources }

let parse xml =
  Some
    { registered_multicast_group_sources =
        Aws.Util.option_bind
          (Aws.Xml.member "registeredMulticastGroupSources" xml)
          TransitGatewayMulticastRegisteredGroupSources.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.registered_multicast_group_sources (fun f ->
             Aws.Query.Pair
               ( "RegisteredMulticastGroupSources"
               , TransitGatewayMulticastRegisteredGroupSources.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.registered_multicast_group_sources (fun f ->
             ( "registeredMulticastGroupSources"
             , TransitGatewayMulticastRegisteredGroupSources.to_json f ))
       ])

let of_json j =
  { registered_multicast_group_sources =
      Aws.Util.option_map
        (Aws.Json.lookup j "registeredMulticastGroupSources")
        TransitGatewayMulticastRegisteredGroupSources.of_json
  }
