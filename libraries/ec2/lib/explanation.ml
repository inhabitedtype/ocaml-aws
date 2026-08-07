open Aws.BaseTypes

type t =
  { acl : AnalysisComponent.t option
  ; acl_rule : AnalysisAclRule.t option
  ; address : String.t option
  ; addresses : IpAddressList.t
  ; attached_to : AnalysisComponent.t option
  ; availability_zones : ValueStringList.t
  ; availability_zone_ids : ValueStringList.t
  ; cidrs : ValueStringList.t
  ; component : AnalysisComponent.t option
  ; customer_gateway : AnalysisComponent.t option
  ; destination : AnalysisComponent.t option
  ; destination_vpc : AnalysisComponent.t option
  ; direction : String.t option
  ; explanation_code : String.t option
  ; ingress_route_table : AnalysisComponent.t option
  ; internet_gateway : AnalysisComponent.t option
  ; load_balancer_arn : String.t option
  ; classic_load_balancer_listener : AnalysisLoadBalancerListener.t option
  ; load_balancer_listener_port : Integer.t option
  ; load_balancer_target : AnalysisLoadBalancerTarget.t option
  ; load_balancer_target_group : AnalysisComponent.t option
  ; load_balancer_target_groups : AnalysisComponentList.t
  ; load_balancer_target_port : Integer.t option
  ; elastic_load_balancer_listener : AnalysisComponent.t option
  ; missing_component : String.t option
  ; nat_gateway : AnalysisComponent.t option
  ; network_interface : AnalysisComponent.t option
  ; packet_field : String.t option
  ; vpc_peering_connection : AnalysisComponent.t option
  ; port : Integer.t option
  ; port_ranges : PortRangeList.t
  ; prefix_list : AnalysisComponent.t option
  ; protocols : StringList.t
  ; route_table_route : AnalysisRouteTableRoute.t option
  ; route_table : AnalysisComponent.t option
  ; security_group : AnalysisComponent.t option
  ; security_group_rule : AnalysisSecurityGroupRule.t option
  ; security_groups : AnalysisComponentList.t
  ; source_vpc : AnalysisComponent.t option
  ; state : String.t option
  ; subnet : AnalysisComponent.t option
  ; subnet_route_table : AnalysisComponent.t option
  ; vpc : AnalysisComponent.t option
  ; vpc_endpoint : AnalysisComponent.t option
  ; vpn_connection : AnalysisComponent.t option
  ; vpn_gateway : AnalysisComponent.t option
  ; transit_gateway : AnalysisComponent.t option
  ; transit_gateway_route_table : AnalysisComponent.t option
  ; transit_gateway_route_table_route : TransitGatewayRouteTableRoute.t option
  ; transit_gateway_attachment : AnalysisComponent.t option
  ; component_account : String.t option
  ; component_region : String.t option
  ; firewall_stateless_rule : FirewallStatelessRule.t option
  ; firewall_stateful_rule : FirewallStatefulRule.t option
  }

let make
    ?acl
    ?acl_rule
    ?address
    ?(addresses = [])
    ?attached_to
    ?(availability_zones = [])
    ?(availability_zone_ids = [])
    ?(cidrs = [])
    ?component
    ?customer_gateway
    ?destination
    ?destination_vpc
    ?direction
    ?explanation_code
    ?ingress_route_table
    ?internet_gateway
    ?load_balancer_arn
    ?classic_load_balancer_listener
    ?load_balancer_listener_port
    ?load_balancer_target
    ?load_balancer_target_group
    ?(load_balancer_target_groups = [])
    ?load_balancer_target_port
    ?elastic_load_balancer_listener
    ?missing_component
    ?nat_gateway
    ?network_interface
    ?packet_field
    ?vpc_peering_connection
    ?port
    ?(port_ranges = [])
    ?prefix_list
    ?(protocols = [])
    ?route_table_route
    ?route_table
    ?security_group
    ?security_group_rule
    ?(security_groups = [])
    ?source_vpc
    ?state
    ?subnet
    ?subnet_route_table
    ?vpc
    ?vpc_endpoint
    ?vpn_connection
    ?vpn_gateway
    ?transit_gateway
    ?transit_gateway_route_table
    ?transit_gateway_route_table_route
    ?transit_gateway_attachment
    ?component_account
    ?component_region
    ?firewall_stateless_rule
    ?firewall_stateful_rule
    () =
  { acl
  ; acl_rule
  ; address
  ; addresses
  ; attached_to
  ; availability_zones
  ; availability_zone_ids
  ; cidrs
  ; component
  ; customer_gateway
  ; destination
  ; destination_vpc
  ; direction
  ; explanation_code
  ; ingress_route_table
  ; internet_gateway
  ; load_balancer_arn
  ; classic_load_balancer_listener
  ; load_balancer_listener_port
  ; load_balancer_target
  ; load_balancer_target_group
  ; load_balancer_target_groups
  ; load_balancer_target_port
  ; elastic_load_balancer_listener
  ; missing_component
  ; nat_gateway
  ; network_interface
  ; packet_field
  ; vpc_peering_connection
  ; port
  ; port_ranges
  ; prefix_list
  ; protocols
  ; route_table_route
  ; route_table
  ; security_group
  ; security_group_rule
  ; security_groups
  ; source_vpc
  ; state
  ; subnet
  ; subnet_route_table
  ; vpc
  ; vpc_endpoint
  ; vpn_connection
  ; vpn_gateway
  ; transit_gateway
  ; transit_gateway_route_table
  ; transit_gateway_route_table_route
  ; transit_gateway_attachment
  ; component_account
  ; component_region
  ; firewall_stateless_rule
  ; firewall_stateful_rule
  }

let parse xml =
  Some
    { acl = Aws.Util.option_bind (Aws.Xml.member "acl" xml) AnalysisComponent.parse
    ; acl_rule = Aws.Util.option_bind (Aws.Xml.member "aclRule" xml) AnalysisAclRule.parse
    ; address = Aws.Util.option_bind (Aws.Xml.member "address" xml) String.parse
    ; addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "addressSet" xml) IpAddressList.parse)
    ; attached_to =
        Aws.Util.option_bind (Aws.Xml.member "attachedTo" xml) AnalysisComponent.parse
    ; availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "availabilityZoneSet" xml)
             ValueStringList.parse)
    ; availability_zone_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "availabilityZoneIdSet" xml)
             ValueStringList.parse)
    ; cidrs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "cidrSet" xml) ValueStringList.parse)
    ; component =
        Aws.Util.option_bind (Aws.Xml.member "component" xml) AnalysisComponent.parse
    ; customer_gateway =
        Aws.Util.option_bind
          (Aws.Xml.member "customerGateway" xml)
          AnalysisComponent.parse
    ; destination =
        Aws.Util.option_bind (Aws.Xml.member "destination" xml) AnalysisComponent.parse
    ; destination_vpc =
        Aws.Util.option_bind (Aws.Xml.member "destinationVpc" xml) AnalysisComponent.parse
    ; direction = Aws.Util.option_bind (Aws.Xml.member "direction" xml) String.parse
    ; explanation_code =
        Aws.Util.option_bind (Aws.Xml.member "explanationCode" xml) String.parse
    ; ingress_route_table =
        Aws.Util.option_bind
          (Aws.Xml.member "ingressRouteTable" xml)
          AnalysisComponent.parse
    ; internet_gateway =
        Aws.Util.option_bind
          (Aws.Xml.member "internetGateway" xml)
          AnalysisComponent.parse
    ; load_balancer_arn =
        Aws.Util.option_bind (Aws.Xml.member "loadBalancerArn" xml) String.parse
    ; classic_load_balancer_listener =
        Aws.Util.option_bind
          (Aws.Xml.member "classicLoadBalancerListener" xml)
          AnalysisLoadBalancerListener.parse
    ; load_balancer_listener_port =
        Aws.Util.option_bind (Aws.Xml.member "loadBalancerListenerPort" xml) Integer.parse
    ; load_balancer_target =
        Aws.Util.option_bind
          (Aws.Xml.member "loadBalancerTarget" xml)
          AnalysisLoadBalancerTarget.parse
    ; load_balancer_target_group =
        Aws.Util.option_bind
          (Aws.Xml.member "loadBalancerTargetGroup" xml)
          AnalysisComponent.parse
    ; load_balancer_target_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "loadBalancerTargetGroupSet" xml)
             AnalysisComponentList.parse)
    ; load_balancer_target_port =
        Aws.Util.option_bind (Aws.Xml.member "loadBalancerTargetPort" xml) Integer.parse
    ; elastic_load_balancer_listener =
        Aws.Util.option_bind
          (Aws.Xml.member "elasticLoadBalancerListener" xml)
          AnalysisComponent.parse
    ; missing_component =
        Aws.Util.option_bind (Aws.Xml.member "missingComponent" xml) String.parse
    ; nat_gateway =
        Aws.Util.option_bind (Aws.Xml.member "natGateway" xml) AnalysisComponent.parse
    ; network_interface =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInterface" xml)
          AnalysisComponent.parse
    ; packet_field = Aws.Util.option_bind (Aws.Xml.member "packetField" xml) String.parse
    ; vpc_peering_connection =
        Aws.Util.option_bind
          (Aws.Xml.member "vpcPeeringConnection" xml)
          AnalysisComponent.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "port" xml) Integer.parse
    ; port_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "portRangeSet" xml) PortRangeList.parse)
    ; prefix_list =
        Aws.Util.option_bind (Aws.Xml.member "prefixList" xml) AnalysisComponent.parse
    ; protocols =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "protocolSet" xml) StringList.parse)
    ; route_table_route =
        Aws.Util.option_bind
          (Aws.Xml.member "routeTableRoute" xml)
          AnalysisRouteTableRoute.parse
    ; route_table =
        Aws.Util.option_bind (Aws.Xml.member "routeTable" xml) AnalysisComponent.parse
    ; security_group =
        Aws.Util.option_bind (Aws.Xml.member "securityGroup" xml) AnalysisComponent.parse
    ; security_group_rule =
        Aws.Util.option_bind
          (Aws.Xml.member "securityGroupRule" xml)
          AnalysisSecurityGroupRule.parse
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupSet" xml)
             AnalysisComponentList.parse)
    ; source_vpc =
        Aws.Util.option_bind (Aws.Xml.member "sourceVpc" xml) AnalysisComponent.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) String.parse
    ; subnet = Aws.Util.option_bind (Aws.Xml.member "subnet" xml) AnalysisComponent.parse
    ; subnet_route_table =
        Aws.Util.option_bind
          (Aws.Xml.member "subnetRouteTable" xml)
          AnalysisComponent.parse
    ; vpc = Aws.Util.option_bind (Aws.Xml.member "vpc" xml) AnalysisComponent.parse
    ; vpc_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpoint" xml) AnalysisComponent.parse
    ; vpn_connection =
        Aws.Util.option_bind (Aws.Xml.member "vpnConnection" xml) AnalysisComponent.parse
    ; vpn_gateway =
        Aws.Util.option_bind (Aws.Xml.member "vpnGateway" xml) AnalysisComponent.parse
    ; transit_gateway =
        Aws.Util.option_bind (Aws.Xml.member "transitGateway" xml) AnalysisComponent.parse
    ; transit_gateway_route_table =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTable" xml)
          AnalysisComponent.parse
    ; transit_gateway_route_table_route =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTableRoute" xml)
          TransitGatewayRouteTableRoute.parse
    ; transit_gateway_attachment =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachment" xml)
          AnalysisComponent.parse
    ; component_account =
        Aws.Util.option_bind (Aws.Xml.member "componentAccount" xml) String.parse
    ; component_region =
        Aws.Util.option_bind (Aws.Xml.member "componentRegion" xml) String.parse
    ; firewall_stateless_rule =
        Aws.Util.option_bind
          (Aws.Xml.member "firewallStatelessRule" xml)
          FirewallStatelessRule.parse
    ; firewall_stateful_rule =
        Aws.Util.option_bind
          (Aws.Xml.member "firewallStatefulRule" xml)
          FirewallStatefulRule.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.firewall_stateful_rule (fun f ->
             Aws.Query.Pair ("FirewallStatefulRule", FirewallStatefulRule.to_query f))
       ; Aws.Util.option_map v.firewall_stateless_rule (fun f ->
             Aws.Query.Pair ("FirewallStatelessRule", FirewallStatelessRule.to_query f))
       ; Aws.Util.option_map v.component_region (fun f ->
             Aws.Query.Pair ("ComponentRegion", String.to_query f))
       ; Aws.Util.option_map v.component_account (fun f ->
             Aws.Query.Pair ("ComponentAccount", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_attachment (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachment", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.transit_gateway_route_table_route (fun f ->
             Aws.Query.Pair
               ("TransitGatewayRouteTableRoute", TransitGatewayRouteTableRoute.to_query f))
       ; Aws.Util.option_map v.transit_gateway_route_table (fun f ->
             Aws.Query.Pair ("TransitGatewayRouteTable", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.transit_gateway (fun f ->
             Aws.Query.Pair ("TransitGateway", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.vpn_gateway (fun f ->
             Aws.Query.Pair ("VpnGateway", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.vpn_connection (fun f ->
             Aws.Query.Pair ("VpnConnection", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint (fun f ->
             Aws.Query.Pair ("VpcEndpoint", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.vpc (fun f ->
             Aws.Query.Pair ("Vpc", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.subnet_route_table (fun f ->
             Aws.Query.Pair ("SubnetRouteTable", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.subnet (fun f ->
             Aws.Query.Pair ("Subnet", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.source_vpc (fun f ->
             Aws.Query.Pair ("SourceVpc", AnalysisComponent.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroupSet", AnalysisComponentList.to_query v.security_groups))
       ; Aws.Util.option_map v.security_group_rule (fun f ->
             Aws.Query.Pair ("SecurityGroupRule", AnalysisSecurityGroupRule.to_query f))
       ; Aws.Util.option_map v.security_group (fun f ->
             Aws.Query.Pair ("SecurityGroup", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.route_table (fun f ->
             Aws.Query.Pair ("RouteTable", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.route_table_route (fun f ->
             Aws.Query.Pair ("RouteTableRoute", AnalysisRouteTableRoute.to_query f))
       ; Some (Aws.Query.Pair ("ProtocolSet", StringList.to_query v.protocols))
       ; Aws.Util.option_map v.prefix_list (fun f ->
             Aws.Query.Pair ("PrefixList", AnalysisComponent.to_query f))
       ; Some (Aws.Query.Pair ("PortRangeSet", PortRangeList.to_query v.port_ranges))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.vpc_peering_connection (fun f ->
             Aws.Query.Pair ("VpcPeeringConnection", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.packet_field (fun f ->
             Aws.Query.Pair ("PacketField", String.to_query f))
       ; Aws.Util.option_map v.network_interface (fun f ->
             Aws.Query.Pair ("NetworkInterface", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.nat_gateway (fun f ->
             Aws.Query.Pair ("NatGateway", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.missing_component (fun f ->
             Aws.Query.Pair ("MissingComponent", String.to_query f))
       ; Aws.Util.option_map v.elastic_load_balancer_listener (fun f ->
             Aws.Query.Pair ("ElasticLoadBalancerListener", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.load_balancer_target_port (fun f ->
             Aws.Query.Pair ("LoadBalancerTargetPort", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LoadBalancerTargetGroupSet"
              , AnalysisComponentList.to_query v.load_balancer_target_groups ))
       ; Aws.Util.option_map v.load_balancer_target_group (fun f ->
             Aws.Query.Pair ("LoadBalancerTargetGroup", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.load_balancer_target (fun f ->
             Aws.Query.Pair ("LoadBalancerTarget", AnalysisLoadBalancerTarget.to_query f))
       ; Aws.Util.option_map v.load_balancer_listener_port (fun f ->
             Aws.Query.Pair ("LoadBalancerListenerPort", Integer.to_query f))
       ; Aws.Util.option_map v.classic_load_balancer_listener (fun f ->
             Aws.Query.Pair
               ("ClassicLoadBalancerListener", AnalysisLoadBalancerListener.to_query f))
       ; Aws.Util.option_map v.load_balancer_arn (fun f ->
             Aws.Query.Pair ("LoadBalancerArn", String.to_query f))
       ; Aws.Util.option_map v.internet_gateway (fun f ->
             Aws.Query.Pair ("InternetGateway", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.ingress_route_table (fun f ->
             Aws.Query.Pair ("IngressRouteTable", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.explanation_code (fun f ->
             Aws.Query.Pair ("ExplanationCode", String.to_query f))
       ; Aws.Util.option_map v.direction (fun f ->
             Aws.Query.Pair ("Direction", String.to_query f))
       ; Aws.Util.option_map v.destination_vpc (fun f ->
             Aws.Query.Pair ("DestinationVpc", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.destination (fun f ->
             Aws.Query.Pair ("Destination", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.customer_gateway (fun f ->
             Aws.Query.Pair ("CustomerGateway", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.component (fun f ->
             Aws.Query.Pair ("Component", AnalysisComponent.to_query f))
       ; Some (Aws.Query.Pair ("CidrSet", ValueStringList.to_query v.cidrs))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZoneIdSet", ValueStringList.to_query v.availability_zone_ids))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZoneSet", ValueStringList.to_query v.availability_zones))
       ; Aws.Util.option_map v.attached_to (fun f ->
             Aws.Query.Pair ("AttachedTo", AnalysisComponent.to_query f))
       ; Some (Aws.Query.Pair ("AddressSet", IpAddressList.to_query v.addresses))
       ; Aws.Util.option_map v.address (fun f ->
             Aws.Query.Pair ("Address", String.to_query f))
       ; Aws.Util.option_map v.acl_rule (fun f ->
             Aws.Query.Pair ("AclRule", AnalysisAclRule.to_query f))
       ; Aws.Util.option_map v.acl (fun f ->
             Aws.Query.Pair ("Acl", AnalysisComponent.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.firewall_stateful_rule (fun f ->
             "firewallStatefulRule", FirewallStatefulRule.to_json f)
       ; Aws.Util.option_map v.firewall_stateless_rule (fun f ->
             "firewallStatelessRule", FirewallStatelessRule.to_json f)
       ; Aws.Util.option_map v.component_region (fun f ->
             "componentRegion", String.to_json f)
       ; Aws.Util.option_map v.component_account (fun f ->
             "componentAccount", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_attachment (fun f ->
             "transitGatewayAttachment", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.transit_gateway_route_table_route (fun f ->
             "transitGatewayRouteTableRoute", TransitGatewayRouteTableRoute.to_json f)
       ; Aws.Util.option_map v.transit_gateway_route_table (fun f ->
             "transitGatewayRouteTable", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.transit_gateway (fun f ->
             "transitGateway", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.vpn_gateway (fun f ->
             "vpnGateway", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.vpn_connection (fun f ->
             "vpnConnection", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint (fun f ->
             "vpcEndpoint", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.vpc (fun f -> "vpc", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.subnet_route_table (fun f ->
             "subnetRouteTable", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.subnet (fun f -> "subnet", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", String.to_json f)
       ; Aws.Util.option_map v.source_vpc (fun f ->
             "sourceVpc", AnalysisComponent.to_json f)
       ; Some ("securityGroupSet", AnalysisComponentList.to_json v.security_groups)
       ; Aws.Util.option_map v.security_group_rule (fun f ->
             "securityGroupRule", AnalysisSecurityGroupRule.to_json f)
       ; Aws.Util.option_map v.security_group (fun f ->
             "securityGroup", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.route_table (fun f ->
             "routeTable", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.route_table_route (fun f ->
             "routeTableRoute", AnalysisRouteTableRoute.to_json f)
       ; Some ("protocolSet", StringList.to_json v.protocols)
       ; Aws.Util.option_map v.prefix_list (fun f ->
             "prefixList", AnalysisComponent.to_json f)
       ; Some ("portRangeSet", PortRangeList.to_json v.port_ranges)
       ; Aws.Util.option_map v.port (fun f -> "port", Integer.to_json f)
       ; Aws.Util.option_map v.vpc_peering_connection (fun f ->
             "vpcPeeringConnection", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.packet_field (fun f -> "packetField", String.to_json f)
       ; Aws.Util.option_map v.network_interface (fun f ->
             "networkInterface", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.nat_gateway (fun f ->
             "natGateway", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.missing_component (fun f ->
             "missingComponent", String.to_json f)
       ; Aws.Util.option_map v.elastic_load_balancer_listener (fun f ->
             "elasticLoadBalancerListener", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.load_balancer_target_port (fun f ->
             "loadBalancerTargetPort", Integer.to_json f)
       ; Some
           ( "loadBalancerTargetGroupSet"
           , AnalysisComponentList.to_json v.load_balancer_target_groups )
       ; Aws.Util.option_map v.load_balancer_target_group (fun f ->
             "loadBalancerTargetGroup", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.load_balancer_target (fun f ->
             "loadBalancerTarget", AnalysisLoadBalancerTarget.to_json f)
       ; Aws.Util.option_map v.load_balancer_listener_port (fun f ->
             "loadBalancerListenerPort", Integer.to_json f)
       ; Aws.Util.option_map v.classic_load_balancer_listener (fun f ->
             "classicLoadBalancerListener", AnalysisLoadBalancerListener.to_json f)
       ; Aws.Util.option_map v.load_balancer_arn (fun f ->
             "loadBalancerArn", String.to_json f)
       ; Aws.Util.option_map v.internet_gateway (fun f ->
             "internetGateway", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.ingress_route_table (fun f ->
             "ingressRouteTable", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.explanation_code (fun f ->
             "explanationCode", String.to_json f)
       ; Aws.Util.option_map v.direction (fun f -> "direction", String.to_json f)
       ; Aws.Util.option_map v.destination_vpc (fun f ->
             "destinationVpc", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.destination (fun f ->
             "destination", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.customer_gateway (fun f ->
             "customerGateway", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.component (fun f ->
             "component", AnalysisComponent.to_json f)
       ; Some ("cidrSet", ValueStringList.to_json v.cidrs)
       ; Some ("availabilityZoneIdSet", ValueStringList.to_json v.availability_zone_ids)
       ; Some ("availabilityZoneSet", ValueStringList.to_json v.availability_zones)
       ; Aws.Util.option_map v.attached_to (fun f ->
             "attachedTo", AnalysisComponent.to_json f)
       ; Some ("addressSet", IpAddressList.to_json v.addresses)
       ; Aws.Util.option_map v.address (fun f -> "address", String.to_json f)
       ; Aws.Util.option_map v.acl_rule (fun f -> "aclRule", AnalysisAclRule.to_json f)
       ; Aws.Util.option_map v.acl (fun f -> "acl", AnalysisComponent.to_json f)
       ])

let of_json j =
  { acl = Aws.Util.option_map (Aws.Json.lookup j "acl") AnalysisComponent.of_json
  ; acl_rule = Aws.Util.option_map (Aws.Json.lookup j "aclRule") AnalysisAclRule.of_json
  ; address = Aws.Util.option_map (Aws.Json.lookup j "address") String.of_json
  ; addresses =
      IpAddressList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "addressSet"))
  ; attached_to =
      Aws.Util.option_map (Aws.Json.lookup j "attachedTo") AnalysisComponent.of_json
  ; availability_zones =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZoneSet"))
  ; availability_zone_ids =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZoneIdSet"))
  ; cidrs = ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "cidrSet"))
  ; component =
      Aws.Util.option_map (Aws.Json.lookup j "component") AnalysisComponent.of_json
  ; customer_gateway =
      Aws.Util.option_map (Aws.Json.lookup j "customerGateway") AnalysisComponent.of_json
  ; destination =
      Aws.Util.option_map (Aws.Json.lookup j "destination") AnalysisComponent.of_json
  ; destination_vpc =
      Aws.Util.option_map (Aws.Json.lookup j "destinationVpc") AnalysisComponent.of_json
  ; direction = Aws.Util.option_map (Aws.Json.lookup j "direction") String.of_json
  ; explanation_code =
      Aws.Util.option_map (Aws.Json.lookup j "explanationCode") String.of_json
  ; ingress_route_table =
      Aws.Util.option_map
        (Aws.Json.lookup j "ingressRouteTable")
        AnalysisComponent.of_json
  ; internet_gateway =
      Aws.Util.option_map (Aws.Json.lookup j "internetGateway") AnalysisComponent.of_json
  ; load_balancer_arn =
      Aws.Util.option_map (Aws.Json.lookup j "loadBalancerArn") String.of_json
  ; classic_load_balancer_listener =
      Aws.Util.option_map
        (Aws.Json.lookup j "classicLoadBalancerListener")
        AnalysisLoadBalancerListener.of_json
  ; load_balancer_listener_port =
      Aws.Util.option_map (Aws.Json.lookup j "loadBalancerListenerPort") Integer.of_json
  ; load_balancer_target =
      Aws.Util.option_map
        (Aws.Json.lookup j "loadBalancerTarget")
        AnalysisLoadBalancerTarget.of_json
  ; load_balancer_target_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "loadBalancerTargetGroup")
        AnalysisComponent.of_json
  ; load_balancer_target_groups =
      AnalysisComponentList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "loadBalancerTargetGroupSet"))
  ; load_balancer_target_port =
      Aws.Util.option_map (Aws.Json.lookup j "loadBalancerTargetPort") Integer.of_json
  ; elastic_load_balancer_listener =
      Aws.Util.option_map
        (Aws.Json.lookup j "elasticLoadBalancerListener")
        AnalysisComponent.of_json
  ; missing_component =
      Aws.Util.option_map (Aws.Json.lookup j "missingComponent") String.of_json
  ; nat_gateway =
      Aws.Util.option_map (Aws.Json.lookup j "natGateway") AnalysisComponent.of_json
  ; network_interface =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterface") AnalysisComponent.of_json
  ; packet_field = Aws.Util.option_map (Aws.Json.lookup j "packetField") String.of_json
  ; vpc_peering_connection =
      Aws.Util.option_map
        (Aws.Json.lookup j "vpcPeeringConnection")
        AnalysisComponent.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "port") Integer.of_json
  ; port_ranges =
      PortRangeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "portRangeSet"))
  ; prefix_list =
      Aws.Util.option_map (Aws.Json.lookup j "prefixList") AnalysisComponent.of_json
  ; protocols =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "protocolSet"))
  ; route_table_route =
      Aws.Util.option_map
        (Aws.Json.lookup j "routeTableRoute")
        AnalysisRouteTableRoute.of_json
  ; route_table =
      Aws.Util.option_map (Aws.Json.lookup j "routeTable") AnalysisComponent.of_json
  ; security_group =
      Aws.Util.option_map (Aws.Json.lookup j "securityGroup") AnalysisComponent.of_json
  ; security_group_rule =
      Aws.Util.option_map
        (Aws.Json.lookup j "securityGroupRule")
        AnalysisSecurityGroupRule.of_json
  ; security_groups =
      AnalysisComponentList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupSet"))
  ; source_vpc =
      Aws.Util.option_map (Aws.Json.lookup j "sourceVpc") AnalysisComponent.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") String.of_json
  ; subnet = Aws.Util.option_map (Aws.Json.lookup j "subnet") AnalysisComponent.of_json
  ; subnet_route_table =
      Aws.Util.option_map (Aws.Json.lookup j "subnetRouteTable") AnalysisComponent.of_json
  ; vpc = Aws.Util.option_map (Aws.Json.lookup j "vpc") AnalysisComponent.of_json
  ; vpc_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpoint") AnalysisComponent.of_json
  ; vpn_connection =
      Aws.Util.option_map (Aws.Json.lookup j "vpnConnection") AnalysisComponent.of_json
  ; vpn_gateway =
      Aws.Util.option_map (Aws.Json.lookup j "vpnGateway") AnalysisComponent.of_json
  ; transit_gateway =
      Aws.Util.option_map (Aws.Json.lookup j "transitGateway") AnalysisComponent.of_json
  ; transit_gateway_route_table =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayRouteTable")
        AnalysisComponent.of_json
  ; transit_gateway_route_table_route =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayRouteTableRoute")
        TransitGatewayRouteTableRoute.of_json
  ; transit_gateway_attachment =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayAttachment")
        AnalysisComponent.of_json
  ; component_account =
      Aws.Util.option_map (Aws.Json.lookup j "componentAccount") String.of_json
  ; component_region =
      Aws.Util.option_map (Aws.Json.lookup j "componentRegion") String.of_json
  ; firewall_stateless_rule =
      Aws.Util.option_map
        (Aws.Json.lookup j "firewallStatelessRule")
        FirewallStatelessRule.of_json
  ; firewall_stateful_rule =
      Aws.Util.option_map
        (Aws.Json.lookup j "firewallStatefulRule")
        FirewallStatefulRule.of_json
  }
