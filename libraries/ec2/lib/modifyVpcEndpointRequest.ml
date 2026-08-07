open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; vpc_endpoint_id : String.t
  ; reset_policy : Boolean.t option
  ; policy_document : String.t option
  ; add_route_table_ids : VpcEndpointRouteTableIdList.t
  ; remove_route_table_ids : VpcEndpointRouteTableIdList.t
  ; add_subnet_ids : VpcEndpointSubnetIdList.t
  ; remove_subnet_ids : VpcEndpointSubnetIdList.t
  ; add_security_group_ids : VpcEndpointSecurityGroupIdList.t
  ; remove_security_group_ids : VpcEndpointSecurityGroupIdList.t
  ; ip_address_type : IpAddressType.t option
  ; dns_options : DnsOptionsSpecification.t option
  ; private_dns_enabled : Boolean.t option
  ; subnet_configurations : SubnetConfigurationsList.t
  }

let make
    ?dry_run
    ~vpc_endpoint_id
    ?reset_policy
    ?policy_document
    ?(add_route_table_ids = [])
    ?(remove_route_table_ids = [])
    ?(add_subnet_ids = [])
    ?(remove_subnet_ids = [])
    ?(add_security_group_ids = [])
    ?(remove_security_group_ids = [])
    ?ip_address_type
    ?dns_options
    ?private_dns_enabled
    ?(subnet_configurations = [])
    () =
  { dry_run
  ; vpc_endpoint_id
  ; reset_policy
  ; policy_document
  ; add_route_table_ids
  ; remove_route_table_ids
  ; add_subnet_ids
  ; remove_subnet_ids
  ; add_security_group_ids
  ; remove_security_group_ids
  ; ip_address_type
  ; dns_options
  ; private_dns_enabled
  ; subnet_configurations
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; vpc_endpoint_id =
        Aws.Xml.required
          "VpcEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "VpcEndpointId" xml) String.parse)
    ; reset_policy = Aws.Util.option_bind (Aws.Xml.member "ResetPolicy" xml) Boolean.parse
    ; policy_document =
        Aws.Util.option_bind (Aws.Xml.member "PolicyDocument" xml) String.parse
    ; add_route_table_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddRouteTableId" xml)
             VpcEndpointRouteTableIdList.parse)
    ; remove_route_table_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveRouteTableId" xml)
             VpcEndpointRouteTableIdList.parse)
    ; add_subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddSubnetId" xml)
             VpcEndpointSubnetIdList.parse)
    ; remove_subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveSubnetId" xml)
             VpcEndpointSubnetIdList.parse)
    ; add_security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddSecurityGroupId" xml)
             VpcEndpointSecurityGroupIdList.parse)
    ; remove_security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveSecurityGroupId" xml)
             VpcEndpointSecurityGroupIdList.parse)
    ; ip_address_type =
        Aws.Util.option_bind (Aws.Xml.member "IpAddressType" xml) IpAddressType.parse
    ; dns_options =
        Aws.Util.option_bind
          (Aws.Xml.member "DnsOptions" xml)
          DnsOptionsSpecification.parse
    ; private_dns_enabled =
        Aws.Util.option_bind (Aws.Xml.member "PrivateDnsEnabled" xml) Boolean.parse
    ; subnet_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetConfiguration" xml)
             SubnetConfigurationsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SubnetConfiguration"
              , SubnetConfigurationsList.to_query v.subnet_configurations ))
       ; Aws.Util.option_map v.private_dns_enabled (fun f ->
             Aws.Query.Pair ("PrivateDnsEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.dns_options (fun f ->
             Aws.Query.Pair ("DnsOptions", DnsOptionsSpecification.to_query f))
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             Aws.Query.Pair ("IpAddressType", IpAddressType.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "RemoveSecurityGroupId"
              , VpcEndpointSecurityGroupIdList.to_query v.remove_security_group_ids ))
       ; Some
           (Aws.Query.Pair
              ( "AddSecurityGroupId"
              , VpcEndpointSecurityGroupIdList.to_query v.add_security_group_ids ))
       ; Some
           (Aws.Query.Pair
              ("RemoveSubnetId", VpcEndpointSubnetIdList.to_query v.remove_subnet_ids))
       ; Some
           (Aws.Query.Pair
              ("AddSubnetId", VpcEndpointSubnetIdList.to_query v.add_subnet_ids))
       ; Some
           (Aws.Query.Pair
              ( "RemoveRouteTableId"
              , VpcEndpointRouteTableIdList.to_query v.remove_route_table_ids ))
       ; Some
           (Aws.Query.Pair
              ( "AddRouteTableId"
              , VpcEndpointRouteTableIdList.to_query v.add_route_table_ids ))
       ; Aws.Util.option_map v.policy_document (fun f ->
             Aws.Query.Pair ("PolicyDocument", String.to_query f))
       ; Aws.Util.option_map v.reset_policy (fun f ->
             Aws.Query.Pair ("ResetPolicy", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("VpcEndpointId", String.to_query v.vpc_endpoint_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "SubnetConfiguration"
           , SubnetConfigurationsList.to_json v.subnet_configurations )
       ; Aws.Util.option_map v.private_dns_enabled (fun f ->
             "PrivateDnsEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.dns_options (fun f ->
             "DnsOptions", DnsOptionsSpecification.to_json f)
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             "IpAddressType", IpAddressType.to_json f)
       ; Some
           ( "RemoveSecurityGroupId"
           , VpcEndpointSecurityGroupIdList.to_json v.remove_security_group_ids )
       ; Some
           ( "AddSecurityGroupId"
           , VpcEndpointSecurityGroupIdList.to_json v.add_security_group_ids )
       ; Some ("RemoveSubnetId", VpcEndpointSubnetIdList.to_json v.remove_subnet_ids)
       ; Some ("AddSubnetId", VpcEndpointSubnetIdList.to_json v.add_subnet_ids)
       ; Some
           ( "RemoveRouteTableId"
           , VpcEndpointRouteTableIdList.to_json v.remove_route_table_ids )
       ; Some
           ("AddRouteTableId", VpcEndpointRouteTableIdList.to_json v.add_route_table_ids)
       ; Aws.Util.option_map v.policy_document (fun f ->
             "PolicyDocument", String.to_json f)
       ; Aws.Util.option_map v.reset_policy (fun f -> "ResetPolicy", Boolean.to_json f)
       ; Some ("VpcEndpointId", String.to_json v.vpc_endpoint_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; vpc_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcEndpointId"))
  ; reset_policy = Aws.Util.option_map (Aws.Json.lookup j "ResetPolicy") Boolean.of_json
  ; policy_document =
      Aws.Util.option_map (Aws.Json.lookup j "PolicyDocument") String.of_json
  ; add_route_table_ids =
      VpcEndpointRouteTableIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddRouteTableId"))
  ; remove_route_table_ids =
      VpcEndpointRouteTableIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveRouteTableId"))
  ; add_subnet_ids =
      VpcEndpointSubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddSubnetId"))
  ; remove_subnet_ids =
      VpcEndpointSubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveSubnetId"))
  ; add_security_group_ids =
      VpcEndpointSecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddSecurityGroupId"))
  ; remove_security_group_ids =
      VpcEndpointSecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveSecurityGroupId"))
  ; ip_address_type =
      Aws.Util.option_map (Aws.Json.lookup j "IpAddressType") IpAddressType.of_json
  ; dns_options =
      Aws.Util.option_map (Aws.Json.lookup j "DnsOptions") DnsOptionsSpecification.of_json
  ; private_dns_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "PrivateDnsEnabled") Boolean.of_json
  ; subnet_configurations =
      SubnetConfigurationsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetConfiguration"))
  }
