open Aws.BaseTypes

type t =
  { sequence_number : Integer.t option
  ; acl_rule : AnalysisAclRule.t option
  ; attached_to : AnalysisComponent.t option
  ; component : AnalysisComponent.t option
  ; destination_vpc : AnalysisComponent.t option
  ; outbound_header : AnalysisPacketHeader.t option
  ; inbound_header : AnalysisPacketHeader.t option
  ; route_table_route : AnalysisRouteTableRoute.t option
  ; security_group_rule : AnalysisSecurityGroupRule.t option
  ; source_vpc : AnalysisComponent.t option
  ; subnet : AnalysisComponent.t option
  ; vpc : AnalysisComponent.t option
  ; additional_details : AdditionalDetailList.t
  ; transit_gateway : AnalysisComponent.t option
  ; transit_gateway_route_table_route : TransitGatewayRouteTableRoute.t option
  ; explanations : ExplanationList.t
  ; elastic_load_balancer_listener : AnalysisComponent.t option
  ; firewall_stateless_rule : FirewallStatelessRule.t option
  ; firewall_stateful_rule : FirewallStatefulRule.t option
  ; service_name : String.t option
  }

let make
    ?sequence_number
    ?acl_rule
    ?attached_to
    ?component
    ?destination_vpc
    ?outbound_header
    ?inbound_header
    ?route_table_route
    ?security_group_rule
    ?source_vpc
    ?subnet
    ?vpc
    ?(additional_details = [])
    ?transit_gateway
    ?transit_gateway_route_table_route
    ?(explanations = [])
    ?elastic_load_balancer_listener
    ?firewall_stateless_rule
    ?firewall_stateful_rule
    ?service_name
    () =
  { sequence_number
  ; acl_rule
  ; attached_to
  ; component
  ; destination_vpc
  ; outbound_header
  ; inbound_header
  ; route_table_route
  ; security_group_rule
  ; source_vpc
  ; subnet
  ; vpc
  ; additional_details
  ; transit_gateway
  ; transit_gateway_route_table_route
  ; explanations
  ; elastic_load_balancer_listener
  ; firewall_stateless_rule
  ; firewall_stateful_rule
  ; service_name
  }

let parse xml =
  Some
    { sequence_number =
        Aws.Util.option_bind (Aws.Xml.member "sequenceNumber" xml) Integer.parse
    ; acl_rule = Aws.Util.option_bind (Aws.Xml.member "aclRule" xml) AnalysisAclRule.parse
    ; attached_to =
        Aws.Util.option_bind (Aws.Xml.member "attachedTo" xml) AnalysisComponent.parse
    ; component =
        Aws.Util.option_bind (Aws.Xml.member "component" xml) AnalysisComponent.parse
    ; destination_vpc =
        Aws.Util.option_bind (Aws.Xml.member "destinationVpc" xml) AnalysisComponent.parse
    ; outbound_header =
        Aws.Util.option_bind
          (Aws.Xml.member "outboundHeader" xml)
          AnalysisPacketHeader.parse
    ; inbound_header =
        Aws.Util.option_bind
          (Aws.Xml.member "inboundHeader" xml)
          AnalysisPacketHeader.parse
    ; route_table_route =
        Aws.Util.option_bind
          (Aws.Xml.member "routeTableRoute" xml)
          AnalysisRouteTableRoute.parse
    ; security_group_rule =
        Aws.Util.option_bind
          (Aws.Xml.member "securityGroupRule" xml)
          AnalysisSecurityGroupRule.parse
    ; source_vpc =
        Aws.Util.option_bind (Aws.Xml.member "sourceVpc" xml) AnalysisComponent.parse
    ; subnet = Aws.Util.option_bind (Aws.Xml.member "subnet" xml) AnalysisComponent.parse
    ; vpc = Aws.Util.option_bind (Aws.Xml.member "vpc" xml) AnalysisComponent.parse
    ; additional_details =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "additionalDetailSet" xml)
             AdditionalDetailList.parse)
    ; transit_gateway =
        Aws.Util.option_bind (Aws.Xml.member "transitGateway" xml) AnalysisComponent.parse
    ; transit_gateway_route_table_route =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTableRoute" xml)
          TransitGatewayRouteTableRoute.parse
    ; explanations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "explanationSet" xml)
             ExplanationList.parse)
    ; elastic_load_balancer_listener =
        Aws.Util.option_bind
          (Aws.Xml.member "elasticLoadBalancerListener" xml)
          AnalysisComponent.parse
    ; firewall_stateless_rule =
        Aws.Util.option_bind
          (Aws.Xml.member "firewallStatelessRule" xml)
          FirewallStatelessRule.parse
    ; firewall_stateful_rule =
        Aws.Util.option_bind
          (Aws.Xml.member "firewallStatefulRule" xml)
          FirewallStatefulRule.parse
    ; service_name = Aws.Util.option_bind (Aws.Xml.member "serviceName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_name (fun f ->
             Aws.Query.Pair ("ServiceName", String.to_query f))
       ; Aws.Util.option_map v.firewall_stateful_rule (fun f ->
             Aws.Query.Pair ("FirewallStatefulRule", FirewallStatefulRule.to_query f))
       ; Aws.Util.option_map v.firewall_stateless_rule (fun f ->
             Aws.Query.Pair ("FirewallStatelessRule", FirewallStatelessRule.to_query f))
       ; Aws.Util.option_map v.elastic_load_balancer_listener (fun f ->
             Aws.Query.Pair ("ElasticLoadBalancerListener", AnalysisComponent.to_query f))
       ; Some (Aws.Query.Pair ("ExplanationSet", ExplanationList.to_query v.explanations))
       ; Aws.Util.option_map v.transit_gateway_route_table_route (fun f ->
             Aws.Query.Pair
               ("TransitGatewayRouteTableRoute", TransitGatewayRouteTableRoute.to_query f))
       ; Aws.Util.option_map v.transit_gateway (fun f ->
             Aws.Query.Pair ("TransitGateway", AnalysisComponent.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AdditionalDetailSet", AdditionalDetailList.to_query v.additional_details))
       ; Aws.Util.option_map v.vpc (fun f ->
             Aws.Query.Pair ("Vpc", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.subnet (fun f ->
             Aws.Query.Pair ("Subnet", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.source_vpc (fun f ->
             Aws.Query.Pair ("SourceVpc", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.security_group_rule (fun f ->
             Aws.Query.Pair ("SecurityGroupRule", AnalysisSecurityGroupRule.to_query f))
       ; Aws.Util.option_map v.route_table_route (fun f ->
             Aws.Query.Pair ("RouteTableRoute", AnalysisRouteTableRoute.to_query f))
       ; Aws.Util.option_map v.inbound_header (fun f ->
             Aws.Query.Pair ("InboundHeader", AnalysisPacketHeader.to_query f))
       ; Aws.Util.option_map v.outbound_header (fun f ->
             Aws.Query.Pair ("OutboundHeader", AnalysisPacketHeader.to_query f))
       ; Aws.Util.option_map v.destination_vpc (fun f ->
             Aws.Query.Pair ("DestinationVpc", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.component (fun f ->
             Aws.Query.Pair ("Component", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.attached_to (fun f ->
             Aws.Query.Pair ("AttachedTo", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.acl_rule (fun f ->
             Aws.Query.Pair ("AclRule", AnalysisAclRule.to_query f))
       ; Aws.Util.option_map v.sequence_number (fun f ->
             Aws.Query.Pair ("SequenceNumber", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_name (fun f -> "serviceName", String.to_json f)
       ; Aws.Util.option_map v.firewall_stateful_rule (fun f ->
             "firewallStatefulRule", FirewallStatefulRule.to_json f)
       ; Aws.Util.option_map v.firewall_stateless_rule (fun f ->
             "firewallStatelessRule", FirewallStatelessRule.to_json f)
       ; Aws.Util.option_map v.elastic_load_balancer_listener (fun f ->
             "elasticLoadBalancerListener", AnalysisComponent.to_json f)
       ; Some ("explanationSet", ExplanationList.to_json v.explanations)
       ; Aws.Util.option_map v.transit_gateway_route_table_route (fun f ->
             "transitGatewayRouteTableRoute", TransitGatewayRouteTableRoute.to_json f)
       ; Aws.Util.option_map v.transit_gateway (fun f ->
             "transitGateway", AnalysisComponent.to_json f)
       ; Some ("additionalDetailSet", AdditionalDetailList.to_json v.additional_details)
       ; Aws.Util.option_map v.vpc (fun f -> "vpc", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.subnet (fun f -> "subnet", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.source_vpc (fun f ->
             "sourceVpc", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.security_group_rule (fun f ->
             "securityGroupRule", AnalysisSecurityGroupRule.to_json f)
       ; Aws.Util.option_map v.route_table_route (fun f ->
             "routeTableRoute", AnalysisRouteTableRoute.to_json f)
       ; Aws.Util.option_map v.inbound_header (fun f ->
             "inboundHeader", AnalysisPacketHeader.to_json f)
       ; Aws.Util.option_map v.outbound_header (fun f ->
             "outboundHeader", AnalysisPacketHeader.to_json f)
       ; Aws.Util.option_map v.destination_vpc (fun f ->
             "destinationVpc", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.component (fun f ->
             "component", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.attached_to (fun f ->
             "attachedTo", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.acl_rule (fun f -> "aclRule", AnalysisAclRule.to_json f)
       ; Aws.Util.option_map v.sequence_number (fun f ->
             "sequenceNumber", Integer.to_json f)
       ])

let of_json j =
  { sequence_number =
      Aws.Util.option_map (Aws.Json.lookup j "sequenceNumber") Integer.of_json
  ; acl_rule = Aws.Util.option_map (Aws.Json.lookup j "aclRule") AnalysisAclRule.of_json
  ; attached_to =
      Aws.Util.option_map (Aws.Json.lookup j "attachedTo") AnalysisComponent.of_json
  ; component =
      Aws.Util.option_map (Aws.Json.lookup j "component") AnalysisComponent.of_json
  ; destination_vpc =
      Aws.Util.option_map (Aws.Json.lookup j "destinationVpc") AnalysisComponent.of_json
  ; outbound_header =
      Aws.Util.option_map
        (Aws.Json.lookup j "outboundHeader")
        AnalysisPacketHeader.of_json
  ; inbound_header =
      Aws.Util.option_map (Aws.Json.lookup j "inboundHeader") AnalysisPacketHeader.of_json
  ; route_table_route =
      Aws.Util.option_map
        (Aws.Json.lookup j "routeTableRoute")
        AnalysisRouteTableRoute.of_json
  ; security_group_rule =
      Aws.Util.option_map
        (Aws.Json.lookup j "securityGroupRule")
        AnalysisSecurityGroupRule.of_json
  ; source_vpc =
      Aws.Util.option_map (Aws.Json.lookup j "sourceVpc") AnalysisComponent.of_json
  ; subnet = Aws.Util.option_map (Aws.Json.lookup j "subnet") AnalysisComponent.of_json
  ; vpc = Aws.Util.option_map (Aws.Json.lookup j "vpc") AnalysisComponent.of_json
  ; additional_details =
      AdditionalDetailList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "additionalDetailSet"))
  ; transit_gateway =
      Aws.Util.option_map (Aws.Json.lookup j "transitGateway") AnalysisComponent.of_json
  ; transit_gateway_route_table_route =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayRouteTableRoute")
        TransitGatewayRouteTableRoute.of_json
  ; explanations =
      ExplanationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "explanationSet"))
  ; elastic_load_balancer_listener =
      Aws.Util.option_map
        (Aws.Json.lookup j "elasticLoadBalancerListener")
        AnalysisComponent.of_json
  ; firewall_stateless_rule =
      Aws.Util.option_map
        (Aws.Json.lookup j "firewallStatelessRule")
        FirewallStatelessRule.of_json
  ; firewall_stateful_rule =
      Aws.Util.option_map
        (Aws.Json.lookup j "firewallStatefulRule")
        FirewallStatefulRule.of_json
  ; service_name = Aws.Util.option_map (Aws.Json.lookup j "serviceName") String.of_json
  }
