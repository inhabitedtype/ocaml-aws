open Aws.BaseTypes

type t =
  { availability_zone_id : String.t option
  ; enable_lni_at_device_index : Integer.t option
  ; map_customer_owned_ip_on_launch : Boolean.t option
  ; customer_owned_ipv4_pool : String.t option
  ; owner_id : String.t option
  ; assign_ipv6_address_on_creation : Boolean.t option
  ; ipv6_cidr_block_association_set : SubnetIpv6CidrBlockAssociationSet.t
  ; tags : TagList.t
  ; subnet_arn : String.t option
  ; outpost_arn : String.t option
  ; enable_dns64 : Boolean.t option
  ; ipv6_native : Boolean.t option
  ; private_dns_name_options_on_launch : PrivateDnsNameOptionsOnLaunch.t option
  ; block_public_access_states : BlockPublicAccessStates.t option
  ; type_ : String.t option
  ; subnet_id : String.t
  ; state : SubnetState.t
  ; vpc_id : String.t
  ; cidr_block : String.t
  ; available_ip_address_count : Integer.t
  ; availability_zone : String.t
  ; default_for_az : Boolean.t
  ; map_public_ip_on_launch : Boolean.t
  }

let make
    ?availability_zone_id
    ?enable_lni_at_device_index
    ?map_customer_owned_ip_on_launch
    ?customer_owned_ipv4_pool
    ?owner_id
    ?assign_ipv6_address_on_creation
    ?(ipv6_cidr_block_association_set = [])
    ?(tags = [])
    ?subnet_arn
    ?outpost_arn
    ?enable_dns64
    ?ipv6_native
    ?private_dns_name_options_on_launch
    ?block_public_access_states
    ?type_
    ~subnet_id
    ~state
    ~vpc_id
    ~cidr_block
    ~available_ip_address_count
    ~availability_zone
    ~default_for_az
    ~map_public_ip_on_launch
    () =
  { availability_zone_id
  ; enable_lni_at_device_index
  ; map_customer_owned_ip_on_launch
  ; customer_owned_ipv4_pool
  ; owner_id
  ; assign_ipv6_address_on_creation
  ; ipv6_cidr_block_association_set
  ; tags
  ; subnet_arn
  ; outpost_arn
  ; enable_dns64
  ; ipv6_native
  ; private_dns_name_options_on_launch
  ; block_public_access_states
  ; type_
  ; subnet_id
  ; state
  ; vpc_id
  ; cidr_block
  ; available_ip_address_count
  ; availability_zone
  ; default_for_az
  ; map_public_ip_on_launch
  }

let parse xml =
  Some
    { availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; enable_lni_at_device_index =
        Aws.Util.option_bind (Aws.Xml.member "enableLniAtDeviceIndex" xml) Integer.parse
    ; map_customer_owned_ip_on_launch =
        Aws.Util.option_bind
          (Aws.Xml.member "mapCustomerOwnedIpOnLaunch" xml)
          Boolean.parse
    ; customer_owned_ipv4_pool =
        Aws.Util.option_bind (Aws.Xml.member "customerOwnedIpv4Pool" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; assign_ipv6_address_on_creation =
        Aws.Util.option_bind
          (Aws.Xml.member "assignIpv6AddressOnCreation" xml)
          Boolean.parse
    ; ipv6_cidr_block_association_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6CidrBlockAssociationSet" xml)
             SubnetIpv6CidrBlockAssociationSet.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; subnet_arn = Aws.Util.option_bind (Aws.Xml.member "subnetArn" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; enable_dns64 = Aws.Util.option_bind (Aws.Xml.member "enableDns64" xml) Boolean.parse
    ; ipv6_native = Aws.Util.option_bind (Aws.Xml.member "ipv6Native" xml) Boolean.parse
    ; private_dns_name_options_on_launch =
        Aws.Util.option_bind
          (Aws.Xml.member "privateDnsNameOptionsOnLaunch" xml)
          PrivateDnsNameOptionsOnLaunch.parse
    ; block_public_access_states =
        Aws.Util.option_bind
          (Aws.Xml.member "blockPublicAccessStates" xml)
          BlockPublicAccessStates.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) String.parse
    ; subnet_id =
        Aws.Xml.required
          "subnetId"
          (Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse)
    ; state =
        Aws.Xml.required
          "state"
          (Aws.Util.option_bind (Aws.Xml.member "state" xml) SubnetState.parse)
    ; vpc_id =
        Aws.Xml.required
          "vpcId"
          (Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse)
    ; cidr_block =
        Aws.Xml.required
          "cidrBlock"
          (Aws.Util.option_bind (Aws.Xml.member "cidrBlock" xml) String.parse)
    ; available_ip_address_count =
        Aws.Xml.required
          "availableIpAddressCount"
          (Aws.Util.option_bind
             (Aws.Xml.member "availableIpAddressCount" xml)
             Integer.parse)
    ; availability_zone =
        Aws.Xml.required
          "availabilityZone"
          (Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse)
    ; default_for_az =
        Aws.Xml.required
          "defaultForAz"
          (Aws.Util.option_bind (Aws.Xml.member "defaultForAz" xml) Boolean.parse)
    ; map_public_ip_on_launch =
        Aws.Xml.required
          "mapPublicIpOnLaunch"
          (Aws.Util.option_bind (Aws.Xml.member "mapPublicIpOnLaunch" xml) Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("MapPublicIpOnLaunch", Boolean.to_query v.map_public_ip_on_launch))
       ; Some (Aws.Query.Pair ("DefaultForAz", Boolean.to_query v.default_for_az))
       ; Some (Aws.Query.Pair ("AvailabilityZone", String.to_query v.availability_zone))
       ; Some
           (Aws.Query.Pair
              ("AvailableIpAddressCount", Integer.to_query v.available_ip_address_count))
       ; Some (Aws.Query.Pair ("CidrBlock", String.to_query v.cidr_block))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Some (Aws.Query.Pair ("State", SubnetState.to_query v.state))
       ; Some (Aws.Query.Pair ("SubnetId", String.to_query v.subnet_id))
       ; Aws.Util.option_map v.type_ (fun f -> Aws.Query.Pair ("Type", String.to_query f))
       ; Aws.Util.option_map v.block_public_access_states (fun f ->
             Aws.Query.Pair ("BlockPublicAccessStates", BlockPublicAccessStates.to_query f))
       ; Aws.Util.option_map v.private_dns_name_options_on_launch (fun f ->
             Aws.Query.Pair
               ("PrivateDnsNameOptionsOnLaunch", PrivateDnsNameOptionsOnLaunch.to_query f))
       ; Aws.Util.option_map v.ipv6_native (fun f ->
             Aws.Query.Pair ("Ipv6Native", Boolean.to_query f))
       ; Aws.Util.option_map v.enable_dns64 (fun f ->
             Aws.Query.Pair ("EnableDns64", Boolean.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.subnet_arn (fun f ->
             Aws.Query.Pair ("SubnetArn", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ( "Ipv6CidrBlockAssociationSet"
              , SubnetIpv6CidrBlockAssociationSet.to_query
                  v.ipv6_cidr_block_association_set ))
       ; Aws.Util.option_map v.assign_ipv6_address_on_creation (fun f ->
             Aws.Query.Pair ("AssignIpv6AddressOnCreation", Boolean.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.customer_owned_ipv4_pool (fun f ->
             Aws.Query.Pair ("CustomerOwnedIpv4Pool", String.to_query f))
       ; Aws.Util.option_map v.map_customer_owned_ip_on_launch (fun f ->
             Aws.Query.Pair ("MapCustomerOwnedIpOnLaunch", Boolean.to_query f))
       ; Aws.Util.option_map v.enable_lni_at_device_index (fun f ->
             Aws.Query.Pair ("EnableLniAtDeviceIndex", Integer.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("mapPublicIpOnLaunch", Boolean.to_json v.map_public_ip_on_launch)
       ; Some ("defaultForAz", Boolean.to_json v.default_for_az)
       ; Some ("availabilityZone", String.to_json v.availability_zone)
       ; Some ("availableIpAddressCount", Integer.to_json v.available_ip_address_count)
       ; Some ("cidrBlock", String.to_json v.cidr_block)
       ; Some ("vpcId", String.to_json v.vpc_id)
       ; Some ("state", SubnetState.to_json v.state)
       ; Some ("subnetId", String.to_json v.subnet_id)
       ; Aws.Util.option_map v.type_ (fun f -> "type", String.to_json f)
       ; Aws.Util.option_map v.block_public_access_states (fun f ->
             "blockPublicAccessStates", BlockPublicAccessStates.to_json f)
       ; Aws.Util.option_map v.private_dns_name_options_on_launch (fun f ->
             "privateDnsNameOptionsOnLaunch", PrivateDnsNameOptionsOnLaunch.to_json f)
       ; Aws.Util.option_map v.ipv6_native (fun f -> "ipv6Native", Boolean.to_json f)
       ; Aws.Util.option_map v.enable_dns64 (fun f -> "enableDns64", Boolean.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.subnet_arn (fun f -> "subnetArn", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Some
           ( "ipv6CidrBlockAssociationSet"
           , SubnetIpv6CidrBlockAssociationSet.to_json v.ipv6_cidr_block_association_set
           )
       ; Aws.Util.option_map v.assign_ipv6_address_on_creation (fun f ->
             "assignIpv6AddressOnCreation", Boolean.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.customer_owned_ipv4_pool (fun f ->
             "customerOwnedIpv4Pool", String.to_json f)
       ; Aws.Util.option_map v.map_customer_owned_ip_on_launch (fun f ->
             "mapCustomerOwnedIpOnLaunch", Boolean.to_json f)
       ; Aws.Util.option_map v.enable_lni_at_device_index (fun f ->
             "enableLniAtDeviceIndex", Integer.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ])

let of_json j =
  { availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; enable_lni_at_device_index =
      Aws.Util.option_map (Aws.Json.lookup j "enableLniAtDeviceIndex") Integer.of_json
  ; map_customer_owned_ip_on_launch =
      Aws.Util.option_map (Aws.Json.lookup j "mapCustomerOwnedIpOnLaunch") Boolean.of_json
  ; customer_owned_ipv4_pool =
      Aws.Util.option_map (Aws.Json.lookup j "customerOwnedIpv4Pool") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; assign_ipv6_address_on_creation =
      Aws.Util.option_map
        (Aws.Json.lookup j "assignIpv6AddressOnCreation")
        Boolean.of_json
  ; ipv6_cidr_block_association_set =
      SubnetIpv6CidrBlockAssociationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6CidrBlockAssociationSet"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; subnet_arn = Aws.Util.option_map (Aws.Json.lookup j "subnetArn") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; enable_dns64 = Aws.Util.option_map (Aws.Json.lookup j "enableDns64") Boolean.of_json
  ; ipv6_native = Aws.Util.option_map (Aws.Json.lookup j "ipv6Native") Boolean.of_json
  ; private_dns_name_options_on_launch =
      Aws.Util.option_map
        (Aws.Json.lookup j "privateDnsNameOptionsOnLaunch")
        PrivateDnsNameOptionsOnLaunch.of_json
  ; block_public_access_states =
      Aws.Util.option_map
        (Aws.Json.lookup j "blockPublicAccessStates")
        BlockPublicAccessStates.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") String.of_json
  ; subnet_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetId"))
  ; state = SubnetState.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "state"))
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcId"))
  ; cidr_block = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "cidrBlock"))
  ; available_ip_address_count =
      Integer.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availableIpAddressCount"))
  ; availability_zone =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZone"))
  ; default_for_az =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "defaultForAz"))
  ; map_public_ip_on_launch =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "mapPublicIpOnLaunch"))
  }
