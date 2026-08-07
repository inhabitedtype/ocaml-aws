open Aws.BaseTypes

type t =
  | Capacity_reservation
  | Client_vpn_endpoint
  | Customer_gateway
  | Carrier_gateway
  | Coip_pool
  | Declarative_policies_report
  | Dedicated_host
  | Dhcp_options
  | Egress_only_internet_gateway
  | Elastic_ip
  | Elastic_gpu
  | Export_image_task
  | Export_instance_task
  | Fleet
  | Fpga_image
  | Host_reservation
  | Image
  | Image_usage_report
  | Import_image_task
  | Import_snapshot_task
  | Instance
  | Instance_event_window
  | Internet_gateway
  | Ipam
  | Ipam_pool
  | Ipam_scope
  | Ipv4pool_ec2
  | Ipv6pool_ec2
  | Key_pair
  | Launch_template
  | Local_gateway
  | Local_gateway_route_table
  | Local_gateway_virtual_interface
  | Local_gateway_virtual_interface_group
  | Local_gateway_route_table_vpc_association
  | Local_gateway_route_table_virtual_interface_group_association
  | Natgateway
  | Network_acl
  | Network_interface
  | Network_insights_analysis
  | Network_insights_path
  | Network_insights_access_scope
  | Network_insights_access_scope_analysis
  | Outpost_lag
  | Placement_group
  | Prefix_list
  | Replace_root_volume_task
  | Reserved_instances
  | Route_table
  | Security_group
  | Security_group_rule
  | Service_link_virtual_interface
  | Snapshot
  | Spot_fleet_request
  | Spot_instances_request
  | Subnet
  | Subnet_cidr_reservation
  | Traffic_mirror_filter
  | Traffic_mirror_session
  | Traffic_mirror_target
  | Transit_gateway
  | Transit_gateway_attachment
  | Transit_gateway_connect_peer
  | Transit_gateway_multicast_domain
  | Transit_gateway_policy_table
  | Transit_gateway_metering_policy
  | Transit_gateway_route_table
  | Transit_gateway_route_table_announcement
  | Volume
  | Vpc
  | Vpc_endpoint
  | Vpc_endpoint_connection
  | Vpc_endpoint_service
  | Vpc_endpoint_service_permission
  | Vpc_peering_connection
  | Vpn_connection
  | Vpn_gateway
  | Vpc_flow_log
  | Capacity_reservation_fleet
  | Traffic_mirror_filter_rule
  | Vpc_endpoint_connection_device_type
  | Verified_access_instance
  | Verified_access_group
  | Verified_access_endpoint
  | Verified_access_policy
  | Verified_access_trust_provider
  | Vpn_connection_device_type
  | Vpc_block_public_access_exclusion
  | Vpc_encryption_control
  | Route_server
  | Route_server_endpoint
  | Route_server_peer
  | Ipam_resource_discovery
  | Ipam_resource_discovery_association
  | Instance_connect_endpoint
  | Verified_access_endpoint_target
  | Ipam_external_resource_verification_token
  | Capacity_block
  | Mac_modification_task
  | Ipam_prefix_list_resolver
  | Ipam_policy
  | Ipam_prefix_list_resolver_target
  | Secondary_interface
  | Secondary_network
  | Secondary_subnet
  | Capacity_manager_data_export
  | Vpn_concentrator
  | Ipam_pool_allocation
  | Capacity_reservation_cancellation_quote

let str_to_t =
  [ "capacity-reservation-cancellation-quote", Capacity_reservation_cancellation_quote
  ; "ipam-pool-allocation", Ipam_pool_allocation
  ; "vpn-concentrator", Vpn_concentrator
  ; "capacity-manager-data-export", Capacity_manager_data_export
  ; "secondary-subnet", Secondary_subnet
  ; "secondary-network", Secondary_network
  ; "secondary-interface", Secondary_interface
  ; "ipam-prefix-list-resolver-target", Ipam_prefix_list_resolver_target
  ; "ipam-policy", Ipam_policy
  ; "ipam-prefix-list-resolver", Ipam_prefix_list_resolver
  ; "mac-modification-task", Mac_modification_task
  ; "capacity-block", Capacity_block
  ; "ipam-external-resource-verification-token", Ipam_external_resource_verification_token
  ; "verified-access-endpoint-target", Verified_access_endpoint_target
  ; "instance-connect-endpoint", Instance_connect_endpoint
  ; "ipam-resource-discovery-association", Ipam_resource_discovery_association
  ; "ipam-resource-discovery", Ipam_resource_discovery
  ; "route-server-peer", Route_server_peer
  ; "route-server-endpoint", Route_server_endpoint
  ; "route-server", Route_server
  ; "vpc-encryption-control", Vpc_encryption_control
  ; "vpc-block-public-access-exclusion", Vpc_block_public_access_exclusion
  ; "vpn-connection-device-type", Vpn_connection_device_type
  ; "verified-access-trust-provider", Verified_access_trust_provider
  ; "verified-access-policy", Verified_access_policy
  ; "verified-access-endpoint", Verified_access_endpoint
  ; "verified-access-group", Verified_access_group
  ; "verified-access-instance", Verified_access_instance
  ; "vpc-endpoint-connection-device-type", Vpc_endpoint_connection_device_type
  ; "traffic-mirror-filter-rule", Traffic_mirror_filter_rule
  ; "capacity-reservation-fleet", Capacity_reservation_fleet
  ; "vpc-flow-log", Vpc_flow_log
  ; "vpn-gateway", Vpn_gateway
  ; "vpn-connection", Vpn_connection
  ; "vpc-peering-connection", Vpc_peering_connection
  ; "vpc-endpoint-service-permission", Vpc_endpoint_service_permission
  ; "vpc-endpoint-service", Vpc_endpoint_service
  ; "vpc-endpoint-connection", Vpc_endpoint_connection
  ; "vpc-endpoint", Vpc_endpoint
  ; "vpc", Vpc
  ; "volume", Volume
  ; "transit-gateway-route-table-announcement", Transit_gateway_route_table_announcement
  ; "transit-gateway-route-table", Transit_gateway_route_table
  ; "transit-gateway-metering-policy", Transit_gateway_metering_policy
  ; "transit-gateway-policy-table", Transit_gateway_policy_table
  ; "transit-gateway-multicast-domain", Transit_gateway_multicast_domain
  ; "transit-gateway-connect-peer", Transit_gateway_connect_peer
  ; "transit-gateway-attachment", Transit_gateway_attachment
  ; "transit-gateway", Transit_gateway
  ; "traffic-mirror-target", Traffic_mirror_target
  ; "traffic-mirror-session", Traffic_mirror_session
  ; "traffic-mirror-filter", Traffic_mirror_filter
  ; "subnet-cidr-reservation", Subnet_cidr_reservation
  ; "subnet", Subnet
  ; "spot-instances-request", Spot_instances_request
  ; "spot-fleet-request", Spot_fleet_request
  ; "snapshot", Snapshot
  ; "service-link-virtual-interface", Service_link_virtual_interface
  ; "security-group-rule", Security_group_rule
  ; "security-group", Security_group
  ; "route-table", Route_table
  ; "reserved-instances", Reserved_instances
  ; "replace-root-volume-task", Replace_root_volume_task
  ; "prefix-list", Prefix_list
  ; "placement-group", Placement_group
  ; "outpost-lag", Outpost_lag
  ; "network-insights-access-scope-analysis", Network_insights_access_scope_analysis
  ; "network-insights-access-scope", Network_insights_access_scope
  ; "network-insights-path", Network_insights_path
  ; "network-insights-analysis", Network_insights_analysis
  ; "network-interface", Network_interface
  ; "network-acl", Network_acl
  ; "natgateway", Natgateway
  ; ( "local-gateway-route-table-virtual-interface-group-association"
    , Local_gateway_route_table_virtual_interface_group_association )
  ; "local-gateway-route-table-vpc-association", Local_gateway_route_table_vpc_association
  ; "local-gateway-virtual-interface-group", Local_gateway_virtual_interface_group
  ; "local-gateway-virtual-interface", Local_gateway_virtual_interface
  ; "local-gateway-route-table", Local_gateway_route_table
  ; "local-gateway", Local_gateway
  ; "launch-template", Launch_template
  ; "key-pair", Key_pair
  ; "ipv6pool-ec2", Ipv6pool_ec2
  ; "ipv4pool-ec2", Ipv4pool_ec2
  ; "ipam-scope", Ipam_scope
  ; "ipam-pool", Ipam_pool
  ; "ipam", Ipam
  ; "internet-gateway", Internet_gateway
  ; "instance-event-window", Instance_event_window
  ; "instance", Instance
  ; "import-snapshot-task", Import_snapshot_task
  ; "import-image-task", Import_image_task
  ; "image-usage-report", Image_usage_report
  ; "image", Image
  ; "host-reservation", Host_reservation
  ; "fpga-image", Fpga_image
  ; "fleet", Fleet
  ; "export-instance-task", Export_instance_task
  ; "export-image-task", Export_image_task
  ; "elastic-gpu", Elastic_gpu
  ; "elastic-ip", Elastic_ip
  ; "egress-only-internet-gateway", Egress_only_internet_gateway
  ; "dhcp-options", Dhcp_options
  ; "dedicated-host", Dedicated_host
  ; "declarative-policies-report", Declarative_policies_report
  ; "coip-pool", Coip_pool
  ; "carrier-gateway", Carrier_gateway
  ; "customer-gateway", Customer_gateway
  ; "client-vpn-endpoint", Client_vpn_endpoint
  ; "capacity-reservation", Capacity_reservation
  ]

let t_to_str =
  [ Capacity_reservation_cancellation_quote, "capacity-reservation-cancellation-quote"
  ; Ipam_pool_allocation, "ipam-pool-allocation"
  ; Vpn_concentrator, "vpn-concentrator"
  ; Capacity_manager_data_export, "capacity-manager-data-export"
  ; Secondary_subnet, "secondary-subnet"
  ; Secondary_network, "secondary-network"
  ; Secondary_interface, "secondary-interface"
  ; Ipam_prefix_list_resolver_target, "ipam-prefix-list-resolver-target"
  ; Ipam_policy, "ipam-policy"
  ; Ipam_prefix_list_resolver, "ipam-prefix-list-resolver"
  ; Mac_modification_task, "mac-modification-task"
  ; Capacity_block, "capacity-block"
  ; Ipam_external_resource_verification_token, "ipam-external-resource-verification-token"
  ; Verified_access_endpoint_target, "verified-access-endpoint-target"
  ; Instance_connect_endpoint, "instance-connect-endpoint"
  ; Ipam_resource_discovery_association, "ipam-resource-discovery-association"
  ; Ipam_resource_discovery, "ipam-resource-discovery"
  ; Route_server_peer, "route-server-peer"
  ; Route_server_endpoint, "route-server-endpoint"
  ; Route_server, "route-server"
  ; Vpc_encryption_control, "vpc-encryption-control"
  ; Vpc_block_public_access_exclusion, "vpc-block-public-access-exclusion"
  ; Vpn_connection_device_type, "vpn-connection-device-type"
  ; Verified_access_trust_provider, "verified-access-trust-provider"
  ; Verified_access_policy, "verified-access-policy"
  ; Verified_access_endpoint, "verified-access-endpoint"
  ; Verified_access_group, "verified-access-group"
  ; Verified_access_instance, "verified-access-instance"
  ; Vpc_endpoint_connection_device_type, "vpc-endpoint-connection-device-type"
  ; Traffic_mirror_filter_rule, "traffic-mirror-filter-rule"
  ; Capacity_reservation_fleet, "capacity-reservation-fleet"
  ; Vpc_flow_log, "vpc-flow-log"
  ; Vpn_gateway, "vpn-gateway"
  ; Vpn_connection, "vpn-connection"
  ; Vpc_peering_connection, "vpc-peering-connection"
  ; Vpc_endpoint_service_permission, "vpc-endpoint-service-permission"
  ; Vpc_endpoint_service, "vpc-endpoint-service"
  ; Vpc_endpoint_connection, "vpc-endpoint-connection"
  ; Vpc_endpoint, "vpc-endpoint"
  ; Vpc, "vpc"
  ; Volume, "volume"
  ; Transit_gateway_route_table_announcement, "transit-gateway-route-table-announcement"
  ; Transit_gateway_route_table, "transit-gateway-route-table"
  ; Transit_gateway_metering_policy, "transit-gateway-metering-policy"
  ; Transit_gateway_policy_table, "transit-gateway-policy-table"
  ; Transit_gateway_multicast_domain, "transit-gateway-multicast-domain"
  ; Transit_gateway_connect_peer, "transit-gateway-connect-peer"
  ; Transit_gateway_attachment, "transit-gateway-attachment"
  ; Transit_gateway, "transit-gateway"
  ; Traffic_mirror_target, "traffic-mirror-target"
  ; Traffic_mirror_session, "traffic-mirror-session"
  ; Traffic_mirror_filter, "traffic-mirror-filter"
  ; Subnet_cidr_reservation, "subnet-cidr-reservation"
  ; Subnet, "subnet"
  ; Spot_instances_request, "spot-instances-request"
  ; Spot_fleet_request, "spot-fleet-request"
  ; Snapshot, "snapshot"
  ; Service_link_virtual_interface, "service-link-virtual-interface"
  ; Security_group_rule, "security-group-rule"
  ; Security_group, "security-group"
  ; Route_table, "route-table"
  ; Reserved_instances, "reserved-instances"
  ; Replace_root_volume_task, "replace-root-volume-task"
  ; Prefix_list, "prefix-list"
  ; Placement_group, "placement-group"
  ; Outpost_lag, "outpost-lag"
  ; Network_insights_access_scope_analysis, "network-insights-access-scope-analysis"
  ; Network_insights_access_scope, "network-insights-access-scope"
  ; Network_insights_path, "network-insights-path"
  ; Network_insights_analysis, "network-insights-analysis"
  ; Network_interface, "network-interface"
  ; Network_acl, "network-acl"
  ; Natgateway, "natgateway"
  ; ( Local_gateway_route_table_virtual_interface_group_association
    , "local-gateway-route-table-virtual-interface-group-association" )
  ; Local_gateway_route_table_vpc_association, "local-gateway-route-table-vpc-association"
  ; Local_gateway_virtual_interface_group, "local-gateway-virtual-interface-group"
  ; Local_gateway_virtual_interface, "local-gateway-virtual-interface"
  ; Local_gateway_route_table, "local-gateway-route-table"
  ; Local_gateway, "local-gateway"
  ; Launch_template, "launch-template"
  ; Key_pair, "key-pair"
  ; Ipv6pool_ec2, "ipv6pool-ec2"
  ; Ipv4pool_ec2, "ipv4pool-ec2"
  ; Ipam_scope, "ipam-scope"
  ; Ipam_pool, "ipam-pool"
  ; Ipam, "ipam"
  ; Internet_gateway, "internet-gateway"
  ; Instance_event_window, "instance-event-window"
  ; Instance, "instance"
  ; Import_snapshot_task, "import-snapshot-task"
  ; Import_image_task, "import-image-task"
  ; Image_usage_report, "image-usage-report"
  ; Image, "image"
  ; Host_reservation, "host-reservation"
  ; Fpga_image, "fpga-image"
  ; Fleet, "fleet"
  ; Export_instance_task, "export-instance-task"
  ; Export_image_task, "export-image-task"
  ; Elastic_gpu, "elastic-gpu"
  ; Elastic_ip, "elastic-ip"
  ; Egress_only_internet_gateway, "egress-only-internet-gateway"
  ; Dhcp_options, "dhcp-options"
  ; Dedicated_host, "dedicated-host"
  ; Declarative_policies_report, "declarative-policies-report"
  ; Coip_pool, "coip-pool"
  ; Carrier_gateway, "carrier-gateway"
  ; Customer_gateway, "customer-gateway"
  ; Client_vpn_endpoint, "client-vpn-endpoint"
  ; Capacity_reservation, "capacity-reservation"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
