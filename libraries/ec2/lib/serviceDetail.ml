open Aws.BaseTypes

type t =
  { service_name : String.t option
  ; service_id : String.t option
  ; service_type : ServiceTypeDetailSet.t
  ; service_region : String.t option
  ; availability_zone_ids : ValueStringList.t
  ; availability_zones : ValueStringList.t
  ; owner : String.t option
  ; base_endpoint_dns_names : ValueStringList.t
  ; private_dns_name : String.t option
  ; private_dns_names : PrivateDnsDetailsSet.t
  ; vpc_endpoint_policy_supported : Boolean.t option
  ; acceptance_required : Boolean.t option
  ; manages_vpc_endpoints : Boolean.t option
  ; payer_responsibility : PayerResponsibility.t option
  ; tags : TagList.t
  ; private_dns_name_verification_state : DnsNameState.t option
  ; supported_ip_address_types : SupportedIpAddressTypes.t
  }

let make
    ?service_name
    ?service_id
    ?(service_type = [])
    ?service_region
    ?(availability_zone_ids = [])
    ?(availability_zones = [])
    ?owner
    ?(base_endpoint_dns_names = [])
    ?private_dns_name
    ?(private_dns_names = [])
    ?vpc_endpoint_policy_supported
    ?acceptance_required
    ?manages_vpc_endpoints
    ?payer_responsibility
    ?(tags = [])
    ?private_dns_name_verification_state
    ?(supported_ip_address_types = [])
    () =
  { service_name
  ; service_id
  ; service_type
  ; service_region
  ; availability_zone_ids
  ; availability_zones
  ; owner
  ; base_endpoint_dns_names
  ; private_dns_name
  ; private_dns_names
  ; vpc_endpoint_policy_supported
  ; acceptance_required
  ; manages_vpc_endpoints
  ; payer_responsibility
  ; tags
  ; private_dns_name_verification_state
  ; supported_ip_address_types
  }

let parse xml =
  Some
    { service_name = Aws.Util.option_bind (Aws.Xml.member "serviceName" xml) String.parse
    ; service_id = Aws.Util.option_bind (Aws.Xml.member "serviceId" xml) String.parse
    ; service_type =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "serviceType" xml)
             ServiceTypeDetailSet.parse)
    ; service_region =
        Aws.Util.option_bind (Aws.Xml.member "serviceRegion" xml) String.parse
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
    ; owner = Aws.Util.option_bind (Aws.Xml.member "owner" xml) String.parse
    ; base_endpoint_dns_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "baseEndpointDnsNameSet" xml)
             ValueStringList.parse)
    ; private_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "privateDnsName" xml) String.parse
    ; private_dns_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "privateDnsNameSet" xml)
             PrivateDnsDetailsSet.parse)
    ; vpc_endpoint_policy_supported =
        Aws.Util.option_bind
          (Aws.Xml.member "vpcEndpointPolicySupported" xml)
          Boolean.parse
    ; acceptance_required =
        Aws.Util.option_bind (Aws.Xml.member "acceptanceRequired" xml) Boolean.parse
    ; manages_vpc_endpoints =
        Aws.Util.option_bind (Aws.Xml.member "managesVpcEndpoints" xml) Boolean.parse
    ; payer_responsibility =
        Aws.Util.option_bind
          (Aws.Xml.member "payerResponsibility" xml)
          PayerResponsibility.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; private_dns_name_verification_state =
        Aws.Util.option_bind
          (Aws.Xml.member "privateDnsNameVerificationState" xml)
          DnsNameState.parse
    ; supported_ip_address_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "supportedIpAddressTypeSet" xml)
             SupportedIpAddressTypes.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SupportedIpAddressTypeSet"
              , SupportedIpAddressTypes.to_query v.supported_ip_address_types ))
       ; Aws.Util.option_map v.private_dns_name_verification_state (fun f ->
             Aws.Query.Pair ("PrivateDnsNameVerificationState", DnsNameState.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.payer_responsibility (fun f ->
             Aws.Query.Pair ("PayerResponsibility", PayerResponsibility.to_query f))
       ; Aws.Util.option_map v.manages_vpc_endpoints (fun f ->
             Aws.Query.Pair ("ManagesVpcEndpoints", Boolean.to_query f))
       ; Aws.Util.option_map v.acceptance_required (fun f ->
             Aws.Query.Pair ("AcceptanceRequired", Boolean.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint_policy_supported (fun f ->
             Aws.Query.Pair ("VpcEndpointPolicySupported", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("PrivateDnsNameSet", PrivateDnsDetailsSet.to_query v.private_dns_names))
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             Aws.Query.Pair ("PrivateDnsName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BaseEndpointDnsNameSet"
              , ValueStringList.to_query v.base_endpoint_dns_names ))
       ; Aws.Util.option_map v.owner (fun f ->
             Aws.Query.Pair ("Owner", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZoneSet", ValueStringList.to_query v.availability_zones))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZoneIdSet", ValueStringList.to_query v.availability_zone_ids))
       ; Aws.Util.option_map v.service_region (fun f ->
             Aws.Query.Pair ("ServiceRegion", String.to_query f))
       ; Some
           (Aws.Query.Pair ("ServiceType", ServiceTypeDetailSet.to_query v.service_type))
       ; Aws.Util.option_map v.service_id (fun f ->
             Aws.Query.Pair ("ServiceId", String.to_query f))
       ; Aws.Util.option_map v.service_name (fun f ->
             Aws.Query.Pair ("ServiceName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "supportedIpAddressTypeSet"
           , SupportedIpAddressTypes.to_json v.supported_ip_address_types )
       ; Aws.Util.option_map v.private_dns_name_verification_state (fun f ->
             "privateDnsNameVerificationState", DnsNameState.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.payer_responsibility (fun f ->
             "payerResponsibility", PayerResponsibility.to_json f)
       ; Aws.Util.option_map v.manages_vpc_endpoints (fun f ->
             "managesVpcEndpoints", Boolean.to_json f)
       ; Aws.Util.option_map v.acceptance_required (fun f ->
             "acceptanceRequired", Boolean.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint_policy_supported (fun f ->
             "vpcEndpointPolicySupported", Boolean.to_json f)
       ; Some ("privateDnsNameSet", PrivateDnsDetailsSet.to_json v.private_dns_names)
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             "privateDnsName", String.to_json f)
       ; Some ("baseEndpointDnsNameSet", ValueStringList.to_json v.base_endpoint_dns_names)
       ; Aws.Util.option_map v.owner (fun f -> "owner", String.to_json f)
       ; Some ("availabilityZoneSet", ValueStringList.to_json v.availability_zones)
       ; Some ("availabilityZoneIdSet", ValueStringList.to_json v.availability_zone_ids)
       ; Aws.Util.option_map v.service_region (fun f -> "serviceRegion", String.to_json f)
       ; Some ("serviceType", ServiceTypeDetailSet.to_json v.service_type)
       ; Aws.Util.option_map v.service_id (fun f -> "serviceId", String.to_json f)
       ; Aws.Util.option_map v.service_name (fun f -> "serviceName", String.to_json f)
       ])

let of_json j =
  { service_name = Aws.Util.option_map (Aws.Json.lookup j "serviceName") String.of_json
  ; service_id = Aws.Util.option_map (Aws.Json.lookup j "serviceId") String.of_json
  ; service_type =
      ServiceTypeDetailSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "serviceType"))
  ; service_region =
      Aws.Util.option_map (Aws.Json.lookup j "serviceRegion") String.of_json
  ; availability_zone_ids =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZoneIdSet"))
  ; availability_zones =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZoneSet"))
  ; owner = Aws.Util.option_map (Aws.Json.lookup j "owner") String.of_json
  ; base_endpoint_dns_names =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "baseEndpointDnsNameSet"))
  ; private_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "privateDnsName") String.of_json
  ; private_dns_names =
      PrivateDnsDetailsSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "privateDnsNameSet"))
  ; vpc_endpoint_policy_supported =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpointPolicySupported") Boolean.of_json
  ; acceptance_required =
      Aws.Util.option_map (Aws.Json.lookup j "acceptanceRequired") Boolean.of_json
  ; manages_vpc_endpoints =
      Aws.Util.option_map (Aws.Json.lookup j "managesVpcEndpoints") Boolean.of_json
  ; payer_responsibility =
      Aws.Util.option_map
        (Aws.Json.lookup j "payerResponsibility")
        PayerResponsibility.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; private_dns_name_verification_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "privateDnsNameVerificationState")
        DnsNameState.of_json
  ; supported_ip_address_types =
      SupportedIpAddressTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "supportedIpAddressTypeSet"))
  }
