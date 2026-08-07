open Aws.BaseTypes

type t =
  { create_time : DateTime.t option
  ; delete_time : DateTime.t option
  ; failure_code : String.t option
  ; failure_message : String.t option
  ; nat_gateway_addresses : NatGatewayAddressList.t
  ; nat_gateway_id : String.t option
  ; provisioned_bandwidth : ProvisionedBandwidth.t option
  ; state : NatGatewayState.t option
  ; subnet_id : String.t option
  ; vpc_id : String.t option
  ; tags : TagList.t
  ; connectivity_type : ConnectivityType.t option
  ; availability_mode : AvailabilityMode.t option
  ; auto_scaling_ips : AutoScalingIpsState.t option
  ; auto_provision_zones : AutoProvisionZonesState.t option
  ; attached_appliances : NatGatewayAttachedApplianceList.t
  ; route_table_id : String.t option
  }

let make
    ?create_time
    ?delete_time
    ?failure_code
    ?failure_message
    ?(nat_gateway_addresses = [])
    ?nat_gateway_id
    ?provisioned_bandwidth
    ?state
    ?subnet_id
    ?vpc_id
    ?(tags = [])
    ?connectivity_type
    ?availability_mode
    ?auto_scaling_ips
    ?auto_provision_zones
    ?(attached_appliances = [])
    ?route_table_id
    () =
  { create_time
  ; delete_time
  ; failure_code
  ; failure_message
  ; nat_gateway_addresses
  ; nat_gateway_id
  ; provisioned_bandwidth
  ; state
  ; subnet_id
  ; vpc_id
  ; tags
  ; connectivity_type
  ; availability_mode
  ; auto_scaling_ips
  ; auto_provision_zones
  ; attached_appliances
  ; route_table_id
  }

let parse xml =
  Some
    { create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; delete_time = Aws.Util.option_bind (Aws.Xml.member "deleteTime" xml) DateTime.parse
    ; failure_code = Aws.Util.option_bind (Aws.Xml.member "failureCode" xml) String.parse
    ; failure_message =
        Aws.Util.option_bind (Aws.Xml.member "failureMessage" xml) String.parse
    ; nat_gateway_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "natGatewayAddressSet" xml)
             NatGatewayAddressList.parse)
    ; nat_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "natGatewayId" xml) String.parse
    ; provisioned_bandwidth =
        Aws.Util.option_bind
          (Aws.Xml.member "provisionedBandwidth" xml)
          ProvisionedBandwidth.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) NatGatewayState.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; connectivity_type =
        Aws.Util.option_bind
          (Aws.Xml.member "connectivityType" xml)
          ConnectivityType.parse
    ; availability_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "availabilityMode" xml)
          AvailabilityMode.parse
    ; auto_scaling_ips =
        Aws.Util.option_bind
          (Aws.Xml.member "autoScalingIps" xml)
          AutoScalingIpsState.parse
    ; auto_provision_zones =
        Aws.Util.option_bind
          (Aws.Xml.member "autoProvisionZones" xml)
          AutoProvisionZonesState.parse
    ; attached_appliances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "attachedApplianceSet" xml)
             NatGatewayAttachedApplianceList.parse)
    ; route_table_id =
        Aws.Util.option_bind (Aws.Xml.member "routeTableId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_table_id (fun f ->
             Aws.Query.Pair ("RouteTableId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AttachedApplianceSet"
              , NatGatewayAttachedApplianceList.to_query v.attached_appliances ))
       ; Aws.Util.option_map v.auto_provision_zones (fun f ->
             Aws.Query.Pair ("AutoProvisionZones", AutoProvisionZonesState.to_query f))
       ; Aws.Util.option_map v.auto_scaling_ips (fun f ->
             Aws.Query.Pair ("AutoScalingIps", AutoScalingIpsState.to_query f))
       ; Aws.Util.option_map v.availability_mode (fun f ->
             Aws.Query.Pair ("AvailabilityMode", AvailabilityMode.to_query f))
       ; Aws.Util.option_map v.connectivity_type (fun f ->
             Aws.Query.Pair ("ConnectivityType", ConnectivityType.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", NatGatewayState.to_query f))
       ; Aws.Util.option_map v.provisioned_bandwidth (fun f ->
             Aws.Query.Pair ("ProvisionedBandwidth", ProvisionedBandwidth.to_query f))
       ; Aws.Util.option_map v.nat_gateway_id (fun f ->
             Aws.Query.Pair ("NatGatewayId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NatGatewayAddressSet"
              , NatGatewayAddressList.to_query v.nat_gateway_addresses ))
       ; Aws.Util.option_map v.failure_message (fun f ->
             Aws.Query.Pair ("FailureMessage", String.to_query f))
       ; Aws.Util.option_map v.failure_code (fun f ->
             Aws.Query.Pair ("FailureCode", String.to_query f))
       ; Aws.Util.option_map v.delete_time (fun f ->
             Aws.Query.Pair ("DeleteTime", DateTime.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_table_id (fun f -> "routeTableId", String.to_json f)
       ; Some
           ( "attachedApplianceSet"
           , NatGatewayAttachedApplianceList.to_json v.attached_appliances )
       ; Aws.Util.option_map v.auto_provision_zones (fun f ->
             "autoProvisionZones", AutoProvisionZonesState.to_json f)
       ; Aws.Util.option_map v.auto_scaling_ips (fun f ->
             "autoScalingIps", AutoScalingIpsState.to_json f)
       ; Aws.Util.option_map v.availability_mode (fun f ->
             "availabilityMode", AvailabilityMode.to_json f)
       ; Aws.Util.option_map v.connectivity_type (fun f ->
             "connectivityType", ConnectivityType.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", NatGatewayState.to_json f)
       ; Aws.Util.option_map v.provisioned_bandwidth (fun f ->
             "provisionedBandwidth", ProvisionedBandwidth.to_json f)
       ; Aws.Util.option_map v.nat_gateway_id (fun f -> "natGatewayId", String.to_json f)
       ; Some
           ("natGatewayAddressSet", NatGatewayAddressList.to_json v.nat_gateway_addresses)
       ; Aws.Util.option_map v.failure_message (fun f ->
             "failureMessage", String.to_json f)
       ; Aws.Util.option_map v.failure_code (fun f -> "failureCode", String.to_json f)
       ; Aws.Util.option_map v.delete_time (fun f -> "deleteTime", DateTime.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ])

let of_json j =
  { create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; delete_time = Aws.Util.option_map (Aws.Json.lookup j "deleteTime") DateTime.of_json
  ; failure_code = Aws.Util.option_map (Aws.Json.lookup j "failureCode") String.of_json
  ; failure_message =
      Aws.Util.option_map (Aws.Json.lookup j "failureMessage") String.of_json
  ; nat_gateway_addresses =
      NatGatewayAddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "natGatewayAddressSet"))
  ; nat_gateway_id = Aws.Util.option_map (Aws.Json.lookup j "natGatewayId") String.of_json
  ; provisioned_bandwidth =
      Aws.Util.option_map
        (Aws.Json.lookup j "provisionedBandwidth")
        ProvisionedBandwidth.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") NatGatewayState.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; connectivity_type =
      Aws.Util.option_map (Aws.Json.lookup j "connectivityType") ConnectivityType.of_json
  ; availability_mode =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityMode") AvailabilityMode.of_json
  ; auto_scaling_ips =
      Aws.Util.option_map (Aws.Json.lookup j "autoScalingIps") AutoScalingIpsState.of_json
  ; auto_provision_zones =
      Aws.Util.option_map
        (Aws.Json.lookup j "autoProvisionZones")
        AutoProvisionZonesState.of_json
  ; attached_appliances =
      NatGatewayAttachedApplianceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "attachedApplianceSet"))
  ; route_table_id = Aws.Util.option_map (Aws.Json.lookup j "routeTableId") String.of_json
  }
