open Aws.BaseTypes

type t =
  { association_id : String.t option
  ; vpc_id : String.t option
  ; target_network_id : String.t option
  ; client_vpn_endpoint_id : String.t option
  ; status : AssociationStatus.t option
  ; security_groups : ValueStringList.t
  ; availability_zones : ClientVpnAvailabilityZoneSet.t
  ; availability_zone_ids : ClientVpnAvailabilityZoneIdSet.t
  }

let make
    ?association_id
    ?vpc_id
    ?target_network_id
    ?client_vpn_endpoint_id
    ?status
    ?(security_groups = [])
    ?(availability_zones = [])
    ?(availability_zone_ids = [])
    () =
  { association_id
  ; vpc_id
  ; target_network_id
  ; client_vpn_endpoint_id
  ; status
  ; security_groups
  ; availability_zones
  ; availability_zone_ids
  }

let parse xml =
  Some
    { association_id =
        Aws.Util.option_bind (Aws.Xml.member "associationId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; target_network_id =
        Aws.Util.option_bind (Aws.Xml.member "targetNetworkId" xml) String.parse
    ; client_vpn_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "clientVpnEndpointId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) AssociationStatus.parse
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroups" xml)
             ValueStringList.parse)
    ; availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "availabilityZoneSet" xml)
             ClientVpnAvailabilityZoneSet.parse)
    ; availability_zone_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "availabilityZoneIdSet" xml)
             ClientVpnAvailabilityZoneIdSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AvailabilityZoneIdSet"
              , ClientVpnAvailabilityZoneIdSet.to_query v.availability_zone_ids ))
       ; Some
           (Aws.Query.Pair
              ( "AvailabilityZoneSet"
              , ClientVpnAvailabilityZoneSet.to_query v.availability_zones ))
       ; Some
           (Aws.Query.Pair ("SecurityGroups", ValueStringList.to_query v.security_groups))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", AssociationStatus.to_query f))
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             Aws.Query.Pair ("ClientVpnEndpointId", String.to_query f))
       ; Aws.Util.option_map v.target_network_id (fun f ->
             Aws.Query.Pair ("TargetNetworkId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.association_id (fun f ->
             Aws.Query.Pair ("AssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "availabilityZoneIdSet"
           , ClientVpnAvailabilityZoneIdSet.to_json v.availability_zone_ids )
       ; Some
           ( "availabilityZoneSet"
           , ClientVpnAvailabilityZoneSet.to_json v.availability_zones )
       ; Some ("securityGroups", ValueStringList.to_json v.security_groups)
       ; Aws.Util.option_map v.status (fun f -> "status", AssociationStatus.to_json f)
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             "clientVpnEndpointId", String.to_json f)
       ; Aws.Util.option_map v.target_network_id (fun f ->
             "targetNetworkId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.association_id (fun f -> "associationId", String.to_json f)
       ])

let of_json j =
  { association_id =
      Aws.Util.option_map (Aws.Json.lookup j "associationId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; target_network_id =
      Aws.Util.option_map (Aws.Json.lookup j "targetNetworkId") String.of_json
  ; client_vpn_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "clientVpnEndpointId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") AssociationStatus.of_json
  ; security_groups =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroups"))
  ; availability_zones =
      ClientVpnAvailabilityZoneSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZoneSet"))
  ; availability_zone_ids =
      ClientVpnAvailabilityZoneIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZoneIdSet"))
  }
