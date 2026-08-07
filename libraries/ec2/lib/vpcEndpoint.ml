open Aws.BaseTypes

type t =
  { vpc_endpoint_id : String.t option
  ; vpc_endpoint_type : VpcEndpointType.t option
  ; vpc_id : String.t option
  ; service_name : String.t option
  ; state : State.t option
  ; policy_document : String.t option
  ; route_table_ids : ValueStringList.t
  ; subnet_ids : ValueStringList.t
  ; groups : GroupIdentifierSet.t
  ; ip_address_type : IpAddressType.t option
  ; dns_options : DnsOptions.t option
  ; private_dns_enabled : Boolean.t option
  ; requester_managed : Boolean.t option
  ; network_interface_ids : ValueStringList.t
  ; dns_entries : DnsEntrySet.t
  ; creation_timestamp : DateTime.t option
  ; tags : TagList.t
  ; owner_id : String.t option
  ; last_error : LastError.t option
  ; ipv4_prefixes : SubnetIpPrefixesList.t
  ; ipv6_prefixes : SubnetIpPrefixesList.t
  ; failure_reason : String.t option
  ; service_network_arn : String.t option
  ; resource_configuration_arn : String.t option
  ; service_region : String.t option
  }

let make
    ?vpc_endpoint_id
    ?vpc_endpoint_type
    ?vpc_id
    ?service_name
    ?state
    ?policy_document
    ?(route_table_ids = [])
    ?(subnet_ids = [])
    ?(groups = [])
    ?ip_address_type
    ?dns_options
    ?private_dns_enabled
    ?requester_managed
    ?(network_interface_ids = [])
    ?(dns_entries = [])
    ?creation_timestamp
    ?(tags = [])
    ?owner_id
    ?last_error
    ?(ipv4_prefixes = [])
    ?(ipv6_prefixes = [])
    ?failure_reason
    ?service_network_arn
    ?resource_configuration_arn
    ?service_region
    () =
  { vpc_endpoint_id
  ; vpc_endpoint_type
  ; vpc_id
  ; service_name
  ; state
  ; policy_document
  ; route_table_ids
  ; subnet_ids
  ; groups
  ; ip_address_type
  ; dns_options
  ; private_dns_enabled
  ; requester_managed
  ; network_interface_ids
  ; dns_entries
  ; creation_timestamp
  ; tags
  ; owner_id
  ; last_error
  ; ipv4_prefixes
  ; ipv6_prefixes
  ; failure_reason
  ; service_network_arn
  ; resource_configuration_arn
  ; service_region
  }

let parse xml =
  Some
    { vpc_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpointId" xml) String.parse
    ; vpc_endpoint_type =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpointType" xml) VpcEndpointType.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; service_name = Aws.Util.option_bind (Aws.Xml.member "serviceName" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) State.parse
    ; policy_document =
        Aws.Util.option_bind (Aws.Xml.member "policyDocument" xml) String.parse
    ; route_table_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "routeTableIdSet" xml)
             ValueStringList.parse)
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "subnetIdSet" xml) ValueStringList.parse)
    ; groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groupSet" xml) GroupIdentifierSet.parse)
    ; ip_address_type =
        Aws.Util.option_bind (Aws.Xml.member "ipAddressType" xml) IpAddressType.parse
    ; dns_options =
        Aws.Util.option_bind (Aws.Xml.member "dnsOptions" xml) DnsOptions.parse
    ; private_dns_enabled =
        Aws.Util.option_bind (Aws.Xml.member "privateDnsEnabled" xml) Boolean.parse
    ; requester_managed =
        Aws.Util.option_bind (Aws.Xml.member "requesterManaged" xml) Boolean.parse
    ; network_interface_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkInterfaceIdSet" xml)
             ValueStringList.parse)
    ; dns_entries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "dnsEntrySet" xml) DnsEntrySet.parse)
    ; creation_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "creationTimestamp" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; last_error = Aws.Util.option_bind (Aws.Xml.member "lastError" xml) LastError.parse
    ; ipv4_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv4PrefixSet" xml)
             SubnetIpPrefixesList.parse)
    ; ipv6_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6PrefixSet" xml)
             SubnetIpPrefixesList.parse)
    ; failure_reason =
        Aws.Util.option_bind (Aws.Xml.member "failureReason" xml) String.parse
    ; service_network_arn =
        Aws.Util.option_bind (Aws.Xml.member "serviceNetworkArn" xml) String.parse
    ; resource_configuration_arn =
        Aws.Util.option_bind (Aws.Xml.member "resourceConfigurationArn" xml) String.parse
    ; service_region =
        Aws.Util.option_bind (Aws.Xml.member "serviceRegion" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_region (fun f ->
             Aws.Query.Pair ("ServiceRegion", String.to_query f))
       ; Aws.Util.option_map v.resource_configuration_arn (fun f ->
             Aws.Query.Pair ("ResourceConfigurationArn", String.to_query f))
       ; Aws.Util.option_map v.service_network_arn (fun f ->
             Aws.Query.Pair ("ServiceNetworkArn", String.to_query f))
       ; Aws.Util.option_map v.failure_reason (fun f ->
             Aws.Query.Pair ("FailureReason", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Ipv6PrefixSet", SubnetIpPrefixesList.to_query v.ipv6_prefixes))
       ; Some
           (Aws.Query.Pair ("Ipv4PrefixSet", SubnetIpPrefixesList.to_query v.ipv4_prefixes))
       ; Aws.Util.option_map v.last_error (fun f ->
             Aws.Query.Pair ("LastError", LastError.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.creation_timestamp (fun f ->
             Aws.Query.Pair ("CreationTimestamp", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("DnsEntrySet", DnsEntrySet.to_query v.dns_entries))
       ; Some
           (Aws.Query.Pair
              ("NetworkInterfaceIdSet", ValueStringList.to_query v.network_interface_ids))
       ; Aws.Util.option_map v.requester_managed (fun f ->
             Aws.Query.Pair ("RequesterManaged", Boolean.to_query f))
       ; Aws.Util.option_map v.private_dns_enabled (fun f ->
             Aws.Query.Pair ("PrivateDnsEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.dns_options (fun f ->
             Aws.Query.Pair ("DnsOptions", DnsOptions.to_query f))
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             Aws.Query.Pair ("IpAddressType", IpAddressType.to_query f))
       ; Some (Aws.Query.Pair ("GroupSet", GroupIdentifierSet.to_query v.groups))
       ; Some (Aws.Query.Pair ("SubnetIdSet", ValueStringList.to_query v.subnet_ids))
       ; Some
           (Aws.Query.Pair ("RouteTableIdSet", ValueStringList.to_query v.route_table_ids))
       ; Aws.Util.option_map v.policy_document (fun f ->
             Aws.Query.Pair ("PolicyDocument", String.to_query f))
       ; Aws.Util.option_map v.state (fun f -> Aws.Query.Pair ("State", State.to_query f))
       ; Aws.Util.option_map v.service_name (fun f ->
             Aws.Query.Pair ("ServiceName", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint_type (fun f ->
             Aws.Query.Pair ("VpcEndpointType", VpcEndpointType.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             Aws.Query.Pair ("VpcEndpointId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_region (fun f -> "serviceRegion", String.to_json f)
       ; Aws.Util.option_map v.resource_configuration_arn (fun f ->
             "resourceConfigurationArn", String.to_json f)
       ; Aws.Util.option_map v.service_network_arn (fun f ->
             "serviceNetworkArn", String.to_json f)
       ; Aws.Util.option_map v.failure_reason (fun f -> "failureReason", String.to_json f)
       ; Some ("ipv6PrefixSet", SubnetIpPrefixesList.to_json v.ipv6_prefixes)
       ; Some ("ipv4PrefixSet", SubnetIpPrefixesList.to_json v.ipv4_prefixes)
       ; Aws.Util.option_map v.last_error (fun f -> "lastError", LastError.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.creation_timestamp (fun f ->
             "creationTimestamp", DateTime.to_json f)
       ; Some ("dnsEntrySet", DnsEntrySet.to_json v.dns_entries)
       ; Some ("networkInterfaceIdSet", ValueStringList.to_json v.network_interface_ids)
       ; Aws.Util.option_map v.requester_managed (fun f ->
             "requesterManaged", Boolean.to_json f)
       ; Aws.Util.option_map v.private_dns_enabled (fun f ->
             "privateDnsEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.dns_options (fun f -> "dnsOptions", DnsOptions.to_json f)
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             "ipAddressType", IpAddressType.to_json f)
       ; Some ("groupSet", GroupIdentifierSet.to_json v.groups)
       ; Some ("subnetIdSet", ValueStringList.to_json v.subnet_ids)
       ; Some ("routeTableIdSet", ValueStringList.to_json v.route_table_ids)
       ; Aws.Util.option_map v.policy_document (fun f ->
             "policyDocument", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", State.to_json f)
       ; Aws.Util.option_map v.service_name (fun f -> "serviceName", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint_type (fun f ->
             "vpcEndpointType", VpcEndpointType.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             "vpcEndpointId", String.to_json f)
       ])

let of_json j =
  { vpc_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpointId") String.of_json
  ; vpc_endpoint_type =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpointType") VpcEndpointType.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; service_name = Aws.Util.option_map (Aws.Json.lookup j "serviceName") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") State.of_json
  ; policy_document =
      Aws.Util.option_map (Aws.Json.lookup j "policyDocument") String.of_json
  ; route_table_ids =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "routeTableIdSet"))
  ; subnet_ids =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetIdSet"))
  ; groups =
      GroupIdentifierSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupSet"))
  ; ip_address_type =
      Aws.Util.option_map (Aws.Json.lookup j "ipAddressType") IpAddressType.of_json
  ; dns_options = Aws.Util.option_map (Aws.Json.lookup j "dnsOptions") DnsOptions.of_json
  ; private_dns_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "privateDnsEnabled") Boolean.of_json
  ; requester_managed =
      Aws.Util.option_map (Aws.Json.lookup j "requesterManaged") Boolean.of_json
  ; network_interface_ids =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceIdSet"))
  ; dns_entries =
      DnsEntrySet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "dnsEntrySet"))
  ; creation_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "creationTimestamp") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; last_error = Aws.Util.option_map (Aws.Json.lookup j "lastError") LastError.of_json
  ; ipv4_prefixes =
      SubnetIpPrefixesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv4PrefixSet"))
  ; ipv6_prefixes =
      SubnetIpPrefixesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6PrefixSet"))
  ; failure_reason =
      Aws.Util.option_map (Aws.Json.lookup j "failureReason") String.of_json
  ; service_network_arn =
      Aws.Util.option_map (Aws.Json.lookup j "serviceNetworkArn") String.of_json
  ; resource_configuration_arn =
      Aws.Util.option_map (Aws.Json.lookup j "resourceConfigurationArn") String.of_json
  ; service_region =
      Aws.Util.option_map (Aws.Json.lookup j "serviceRegion") String.of_json
  }
