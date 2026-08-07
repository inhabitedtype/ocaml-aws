open Aws.BaseTypes

type t =
  { interconnect_subnet_id : String.t option
  ; transit_gateway_multicast_domain_id : String.t option
  ; replica_mode : ReplicaMode.t option
  }

let make ?interconnect_subnet_id ?transit_gateway_multicast_domain_id ?replica_mode () =
  { interconnect_subnet_id; transit_gateway_multicast_domain_id; replica_mode }

let parse xml =
  Some
    { interconnect_subnet_id =
        Aws.Util.option_bind (Aws.Xml.member "InterconnectSubnetId" xml) String.parse
    ; transit_gateway_multicast_domain_id =
        Aws.Util.option_bind
          (Aws.Xml.member "TransitGatewayMulticastDomainId" xml)
          String.parse
    ; replica_mode =
        Aws.Util.option_bind (Aws.Xml.member "ReplicaMode" xml) ReplicaMode.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.replica_mode (fun f ->
             Aws.Query.Pair ("ReplicaMode", ReplicaMode.to_query f))
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_id (fun f ->
             Aws.Query.Pair ("TransitGatewayMulticastDomainId", String.to_query f))
       ; Aws.Util.option_map v.interconnect_subnet_id (fun f ->
             Aws.Query.Pair ("InterconnectSubnetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.replica_mode (fun f ->
             "ReplicaMode", ReplicaMode.to_json f)
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_id (fun f ->
             "TransitGatewayMulticastDomainId", String.to_json f)
       ; Aws.Util.option_map v.interconnect_subnet_id (fun f ->
             "InterconnectSubnetId", String.to_json f)
       ])

let of_json j =
  { interconnect_subnet_id =
      Aws.Util.option_map (Aws.Json.lookup j "InterconnectSubnetId") String.of_json
  ; transit_gateway_multicast_domain_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "TransitGatewayMulticastDomainId")
        String.of_json
  ; replica_mode =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicaMode") ReplicaMode.of_json
  }
