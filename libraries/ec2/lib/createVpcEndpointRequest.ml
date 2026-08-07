open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; vpc_endpoint_type : VpcEndpointType.t option
  ; vpc_id : String.t
  ; service_name : String.t option
  ; policy_document : String.t option
  ; route_table_ids : VpcEndpointRouteTableIdList.t
  ; subnet_ids : VpcEndpointSubnetIdList.t
  ; security_group_ids : VpcEndpointSecurityGroupIdList.t
  ; ip_address_type : IpAddressType.t option
  ; dns_options : DnsOptionsSpecification.t option
  ; client_token : String.t option
  ; private_dns_enabled : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  ; subnet_configurations : SubnetConfigurationsList.t
  ; service_network_arn : String.t option
  ; resource_configuration_arn : String.t option
  ; service_region : String.t option
  }

let make
    ?dry_run
    ?vpc_endpoint_type
    ~vpc_id
    ?service_name
    ?policy_document
    ?(route_table_ids = [])
    ?(subnet_ids = [])
    ?(security_group_ids = [])
    ?ip_address_type
    ?dns_options
    ?client_token
    ?private_dns_enabled
    ?(tag_specifications = [])
    ?(subnet_configurations = [])
    ?service_network_arn
    ?resource_configuration_arn
    ?service_region
    () =
  { dry_run
  ; vpc_endpoint_type
  ; vpc_id
  ; service_name
  ; policy_document
  ; route_table_ids
  ; subnet_ids
  ; security_group_ids
  ; ip_address_type
  ; dns_options
  ; client_token
  ; private_dns_enabled
  ; tag_specifications
  ; subnet_configurations
  ; service_network_arn
  ; resource_configuration_arn
  ; service_region
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; vpc_endpoint_type =
        Aws.Util.option_bind (Aws.Xml.member "VpcEndpointType" xml) VpcEndpointType.parse
    ; vpc_id =
        Aws.Xml.required
          "VpcId"
          (Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse)
    ; service_name = Aws.Util.option_bind (Aws.Xml.member "ServiceName" xml) String.parse
    ; policy_document =
        Aws.Util.option_bind (Aws.Xml.member "PolicyDocument" xml) String.parse
    ; route_table_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RouteTableId" xml)
             VpcEndpointRouteTableIdList.parse)
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetId" xml)
             VpcEndpointSubnetIdList.parse)
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             VpcEndpointSecurityGroupIdList.parse)
    ; ip_address_type =
        Aws.Util.option_bind (Aws.Xml.member "IpAddressType" xml) IpAddressType.parse
    ; dns_options =
        Aws.Util.option_bind
          (Aws.Xml.member "DnsOptions" xml)
          DnsOptionsSpecification.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; private_dns_enabled =
        Aws.Util.option_bind (Aws.Xml.member "PrivateDnsEnabled" xml) Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; subnet_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetConfiguration" xml)
             SubnetConfigurationsList.parse)
    ; service_network_arn =
        Aws.Util.option_bind (Aws.Xml.member "ServiceNetworkArn" xml) String.parse
    ; resource_configuration_arn =
        Aws.Util.option_bind (Aws.Xml.member "ResourceConfigurationArn" xml) String.parse
    ; service_region =
        Aws.Util.option_bind (Aws.Xml.member "ServiceRegion" xml) String.parse
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
       ; Some
           (Aws.Query.Pair
              ( "SubnetConfiguration"
              , SubnetConfigurationsList.to_query v.subnet_configurations ))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.private_dns_enabled (fun f ->
             Aws.Query.Pair ("PrivateDnsEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dns_options (fun f ->
             Aws.Query.Pair ("DnsOptions", DnsOptionsSpecification.to_query f))
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             Aws.Query.Pair ("IpAddressType", IpAddressType.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupId"
              , VpcEndpointSecurityGroupIdList.to_query v.security_group_ids ))
       ; Some (Aws.Query.Pair ("SubnetId", VpcEndpointSubnetIdList.to_query v.subnet_ids))
       ; Some
           (Aws.Query.Pair
              ("RouteTableId", VpcEndpointRouteTableIdList.to_query v.route_table_ids))
       ; Aws.Util.option_map v.policy_document (fun f ->
             Aws.Query.Pair ("PolicyDocument", String.to_query f))
       ; Aws.Util.option_map v.service_name (fun f ->
             Aws.Query.Pair ("ServiceName", String.to_query f))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Aws.Util.option_map v.vpc_endpoint_type (fun f ->
             Aws.Query.Pair ("VpcEndpointType", VpcEndpointType.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_region (fun f -> "ServiceRegion", String.to_json f)
       ; Aws.Util.option_map v.resource_configuration_arn (fun f ->
             "ResourceConfigurationArn", String.to_json f)
       ; Aws.Util.option_map v.service_network_arn (fun f ->
             "ServiceNetworkArn", String.to_json f)
       ; Some
           ( "SubnetConfiguration"
           , SubnetConfigurationsList.to_json v.subnet_configurations )
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.private_dns_enabled (fun f ->
             "PrivateDnsEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dns_options (fun f ->
             "DnsOptions", DnsOptionsSpecification.to_json f)
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             "IpAddressType", IpAddressType.to_json f)
       ; Some
           ("SecurityGroupId", VpcEndpointSecurityGroupIdList.to_json v.security_group_ids)
       ; Some ("SubnetId", VpcEndpointSubnetIdList.to_json v.subnet_ids)
       ; Some ("RouteTableId", VpcEndpointRouteTableIdList.to_json v.route_table_ids)
       ; Aws.Util.option_map v.policy_document (fun f ->
             "PolicyDocument", String.to_json f)
       ; Aws.Util.option_map v.service_name (fun f -> "ServiceName", String.to_json f)
       ; Some ("VpcId", String.to_json v.vpc_id)
       ; Aws.Util.option_map v.vpc_endpoint_type (fun f ->
             "VpcEndpointType", VpcEndpointType.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; vpc_endpoint_type =
      Aws.Util.option_map (Aws.Json.lookup j "VpcEndpointType") VpcEndpointType.of_json
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcId"))
  ; service_name = Aws.Util.option_map (Aws.Json.lookup j "ServiceName") String.of_json
  ; policy_document =
      Aws.Util.option_map (Aws.Json.lookup j "PolicyDocument") String.of_json
  ; route_table_ids =
      VpcEndpointRouteTableIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RouteTableId"))
  ; subnet_ids =
      VpcEndpointSubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetId"))
  ; security_group_ids =
      VpcEndpointSecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; ip_address_type =
      Aws.Util.option_map (Aws.Json.lookup j "IpAddressType") IpAddressType.of_json
  ; dns_options =
      Aws.Util.option_map (Aws.Json.lookup j "DnsOptions") DnsOptionsSpecification.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; private_dns_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "PrivateDnsEnabled") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; subnet_configurations =
      SubnetConfigurationsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetConfiguration"))
  ; service_network_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceNetworkArn") String.of_json
  ; resource_configuration_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceConfigurationArn") String.of_json
  ; service_region =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceRegion") String.of_json
  }
