open Aws.BaseTypes

type t =
  { service_type : ServiceTypeDetailSet.t
  ; service_id : String.t option
  ; service_name : String.t option
  ; service_state : ServiceState.t option
  ; availability_zone_ids : ValueStringList.t
  ; availability_zones : ValueStringList.t
  ; acceptance_required : Boolean.t option
  ; manages_vpc_endpoints : Boolean.t option
  ; network_load_balancer_arns : ValueStringList.t
  ; gateway_load_balancer_arns : ValueStringList.t
  ; supported_ip_address_types : SupportedIpAddressTypes.t
  ; base_endpoint_dns_names : ValueStringList.t
  ; private_dns_name : String.t option
  ; private_dns_name_configuration : PrivateDnsNameConfiguration.t option
  ; payer_responsibility : PayerResponsibility.t option
  ; tags : TagList.t
  ; supported_regions : SupportedRegionSet.t
  ; remote_access_enabled : Boolean.t option
  }

let make
    ?(service_type = [])
    ?service_id
    ?service_name
    ?service_state
    ?(availability_zone_ids = [])
    ?(availability_zones = [])
    ?acceptance_required
    ?manages_vpc_endpoints
    ?(network_load_balancer_arns = [])
    ?(gateway_load_balancer_arns = [])
    ?(supported_ip_address_types = [])
    ?(base_endpoint_dns_names = [])
    ?private_dns_name
    ?private_dns_name_configuration
    ?payer_responsibility
    ?(tags = [])
    ?(supported_regions = [])
    ?remote_access_enabled
    () =
  { service_type
  ; service_id
  ; service_name
  ; service_state
  ; availability_zone_ids
  ; availability_zones
  ; acceptance_required
  ; manages_vpc_endpoints
  ; network_load_balancer_arns
  ; gateway_load_balancer_arns
  ; supported_ip_address_types
  ; base_endpoint_dns_names
  ; private_dns_name
  ; private_dns_name_configuration
  ; payer_responsibility
  ; tags
  ; supported_regions
  ; remote_access_enabled
  }

let parse xml =
  Some
    { service_type =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "serviceType" xml)
             ServiceTypeDetailSet.parse)
    ; service_id = Aws.Util.option_bind (Aws.Xml.member "serviceId" xml) String.parse
    ; service_name = Aws.Util.option_bind (Aws.Xml.member "serviceName" xml) String.parse
    ; service_state =
        Aws.Util.option_bind (Aws.Xml.member "serviceState" xml) ServiceState.parse
    ; availability_zone_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "availabilityZoneIdSet" xml)
             ValueStringList.parse)
    ; availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "availabilityZoneSet" xml)
             ValueStringList.parse)
    ; acceptance_required =
        Aws.Util.option_bind (Aws.Xml.member "acceptanceRequired" xml) Boolean.parse
    ; manages_vpc_endpoints =
        Aws.Util.option_bind (Aws.Xml.member "managesVpcEndpoints" xml) Boolean.parse
    ; network_load_balancer_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkLoadBalancerArnSet" xml)
             ValueStringList.parse)
    ; gateway_load_balancer_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "gatewayLoadBalancerArnSet" xml)
             ValueStringList.parse)
    ; supported_ip_address_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "supportedIpAddressTypeSet" xml)
             SupportedIpAddressTypes.parse)
    ; base_endpoint_dns_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "baseEndpointDnsNameSet" xml)
             ValueStringList.parse)
    ; private_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "privateDnsName" xml) String.parse
    ; private_dns_name_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "privateDnsNameConfiguration" xml)
          PrivateDnsNameConfiguration.parse
    ; payer_responsibility =
        Aws.Util.option_bind
          (Aws.Xml.member "payerResponsibility" xml)
          PayerResponsibility.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; supported_regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "supportedRegionSet" xml)
             SupportedRegionSet.parse)
    ; remote_access_enabled =
        Aws.Util.option_bind (Aws.Xml.member "remoteAccessEnabled" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.remote_access_enabled (fun f ->
             Aws.Query.Pair ("RemoteAccessEnabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SupportedRegionSet", SupportedRegionSet.to_query v.supported_regions))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.payer_responsibility (fun f ->
             Aws.Query.Pair ("PayerResponsibility", PayerResponsibility.to_query f))
       ; Aws.Util.option_map v.private_dns_name_configuration (fun f ->
             Aws.Query.Pair
               ("PrivateDnsNameConfiguration", PrivateDnsNameConfiguration.to_query f))
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             Aws.Query.Pair ("PrivateDnsName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BaseEndpointDnsNameSet"
              , ValueStringList.to_query v.base_endpoint_dns_names ))
       ; Some
           (Aws.Query.Pair
              ( "SupportedIpAddressTypeSet"
              , SupportedIpAddressTypes.to_query v.supported_ip_address_types ))
       ; Some
           (Aws.Query.Pair
              ( "GatewayLoadBalancerArnSet"
              , ValueStringList.to_query v.gateway_load_balancer_arns ))
       ; Some
           (Aws.Query.Pair
              ( "NetworkLoadBalancerArnSet"
              , ValueStringList.to_query v.network_load_balancer_arns ))
       ; Aws.Util.option_map v.manages_vpc_endpoints (fun f ->
             Aws.Query.Pair ("ManagesVpcEndpoints", Boolean.to_query f))
       ; Aws.Util.option_map v.acceptance_required (fun f ->
             Aws.Query.Pair ("AcceptanceRequired", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZoneSet", ValueStringList.to_query v.availability_zones))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZoneIdSet", ValueStringList.to_query v.availability_zone_ids))
       ; Aws.Util.option_map v.service_state (fun f ->
             Aws.Query.Pair ("ServiceState", ServiceState.to_query f))
       ; Aws.Util.option_map v.service_name (fun f ->
             Aws.Query.Pair ("ServiceName", String.to_query f))
       ; Aws.Util.option_map v.service_id (fun f ->
             Aws.Query.Pair ("ServiceId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("ServiceType", ServiceTypeDetailSet.to_query v.service_type))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.remote_access_enabled (fun f ->
             "remoteAccessEnabled", Boolean.to_json f)
       ; Some ("supportedRegionSet", SupportedRegionSet.to_json v.supported_regions)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.payer_responsibility (fun f ->
             "payerResponsibility", PayerResponsibility.to_json f)
       ; Aws.Util.option_map v.private_dns_name_configuration (fun f ->
             "privateDnsNameConfiguration", PrivateDnsNameConfiguration.to_json f)
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             "privateDnsName", String.to_json f)
       ; Some ("baseEndpointDnsNameSet", ValueStringList.to_json v.base_endpoint_dns_names)
       ; Some
           ( "supportedIpAddressTypeSet"
           , SupportedIpAddressTypes.to_json v.supported_ip_address_types )
       ; Some
           ( "gatewayLoadBalancerArnSet"
           , ValueStringList.to_json v.gateway_load_balancer_arns )
       ; Some
           ( "networkLoadBalancerArnSet"
           , ValueStringList.to_json v.network_load_balancer_arns )
       ; Aws.Util.option_map v.manages_vpc_endpoints (fun f ->
             "managesVpcEndpoints", Boolean.to_json f)
       ; Aws.Util.option_map v.acceptance_required (fun f ->
             "acceptanceRequired", Boolean.to_json f)
       ; Some ("availabilityZoneSet", ValueStringList.to_json v.availability_zones)
       ; Some ("availabilityZoneIdSet", ValueStringList.to_json v.availability_zone_ids)
       ; Aws.Util.option_map v.service_state (fun f ->
             "serviceState", ServiceState.to_json f)
       ; Aws.Util.option_map v.service_name (fun f -> "serviceName", String.to_json f)
       ; Aws.Util.option_map v.service_id (fun f -> "serviceId", String.to_json f)
       ; Some ("serviceType", ServiceTypeDetailSet.to_json v.service_type)
       ])

let of_json j =
  { service_type =
      ServiceTypeDetailSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "serviceType"))
  ; service_id = Aws.Util.option_map (Aws.Json.lookup j "serviceId") String.of_json
  ; service_name = Aws.Util.option_map (Aws.Json.lookup j "serviceName") String.of_json
  ; service_state =
      Aws.Util.option_map (Aws.Json.lookup j "serviceState") ServiceState.of_json
  ; availability_zone_ids =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZoneIdSet"))
  ; availability_zones =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZoneSet"))
  ; acceptance_required =
      Aws.Util.option_map (Aws.Json.lookup j "acceptanceRequired") Boolean.of_json
  ; manages_vpc_endpoints =
      Aws.Util.option_map (Aws.Json.lookup j "managesVpcEndpoints") Boolean.of_json
  ; network_load_balancer_arns =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkLoadBalancerArnSet"))
  ; gateway_load_balancer_arns =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "gatewayLoadBalancerArnSet"))
  ; supported_ip_address_types =
      SupportedIpAddressTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "supportedIpAddressTypeSet"))
  ; base_endpoint_dns_names =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "baseEndpointDnsNameSet"))
  ; private_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "privateDnsName") String.of_json
  ; private_dns_name_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "privateDnsNameConfiguration")
        PrivateDnsNameConfiguration.of_json
  ; payer_responsibility =
      Aws.Util.option_map
        (Aws.Json.lookup j "payerResponsibility")
        PayerResponsibility.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; supported_regions =
      SupportedRegionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "supportedRegionSet"))
  ; remote_access_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "remoteAccessEnabled") Boolean.of_json
  }
