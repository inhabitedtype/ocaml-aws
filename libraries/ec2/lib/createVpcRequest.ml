open Aws.BaseTypes

type t =
  { cidr_block : String.t option
  ; ipv6_pool : String.t option
  ; ipv6_cidr_block : String.t option
  ; ipv4_ipam_pool_id : String.t option
  ; ipv4_netmask_length : Integer.t option
  ; ipv6_ipam_pool_id : String.t option
  ; ipv6_netmask_length : Integer.t option
  ; ipv6_cidr_block_network_border_group : String.t option
  ; vpc_encryption_control : VpcEncryptionControlConfiguration.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  ; instance_tenancy : Tenancy.t option
  ; amazon_provided_ipv6_cidr_block : Boolean.t option
  }

let make
    ?cidr_block
    ?ipv6_pool
    ?ipv6_cidr_block
    ?ipv4_ipam_pool_id
    ?ipv4_netmask_length
    ?ipv6_ipam_pool_id
    ?ipv6_netmask_length
    ?ipv6_cidr_block_network_border_group
    ?vpc_encryption_control
    ?(tag_specifications = [])
    ?dry_run
    ?instance_tenancy
    ?amazon_provided_ipv6_cidr_block
    () =
  { cidr_block
  ; ipv6_pool
  ; ipv6_cidr_block
  ; ipv4_ipam_pool_id
  ; ipv4_netmask_length
  ; ipv6_ipam_pool_id
  ; ipv6_netmask_length
  ; ipv6_cidr_block_network_border_group
  ; vpc_encryption_control
  ; tag_specifications
  ; dry_run
  ; instance_tenancy
  ; amazon_provided_ipv6_cidr_block
  }

let parse xml =
  Some
    { cidr_block = Aws.Util.option_bind (Aws.Xml.member "CidrBlock" xml) String.parse
    ; ipv6_pool = Aws.Util.option_bind (Aws.Xml.member "Ipv6Pool" xml) String.parse
    ; ipv6_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6CidrBlock" xml) String.parse
    ; ipv4_ipam_pool_id =
        Aws.Util.option_bind (Aws.Xml.member "Ipv4IpamPoolId" xml) String.parse
    ; ipv4_netmask_length =
        Aws.Util.option_bind (Aws.Xml.member "Ipv4NetmaskLength" xml) Integer.parse
    ; ipv6_ipam_pool_id =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6IpamPoolId" xml) String.parse
    ; ipv6_netmask_length =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6NetmaskLength" xml) Integer.parse
    ; ipv6_cidr_block_network_border_group =
        Aws.Util.option_bind
          (Aws.Xml.member "Ipv6CidrBlockNetworkBorderGroup" xml)
          String.parse
    ; vpc_encryption_control =
        Aws.Util.option_bind
          (Aws.Xml.member "VpcEncryptionControl" xml)
          VpcEncryptionControlConfiguration.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; instance_tenancy =
        Aws.Util.option_bind (Aws.Xml.member "instanceTenancy" xml) Tenancy.parse
    ; amazon_provided_ipv6_cidr_block =
        Aws.Util.option_bind
          (Aws.Xml.member "amazonProvidedIpv6CidrBlock" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.amazon_provided_ipv6_cidr_block (fun f ->
             Aws.Query.Pair ("AmazonProvidedIpv6CidrBlock", Boolean.to_query f))
       ; Aws.Util.option_map v.instance_tenancy (fun f ->
             Aws.Query.Pair ("InstanceTenancy", Tenancy.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.vpc_encryption_control (fun f ->
             Aws.Query.Pair
               ("VpcEncryptionControl", VpcEncryptionControlConfiguration.to_query f))
       ; Aws.Util.option_map v.ipv6_cidr_block_network_border_group (fun f ->
             Aws.Query.Pair ("Ipv6CidrBlockNetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.ipv6_netmask_length (fun f ->
             Aws.Query.Pair ("Ipv6NetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.ipv6_ipam_pool_id (fun f ->
             Aws.Query.Pair ("Ipv6IpamPoolId", String.to_query f))
       ; Aws.Util.option_map v.ipv4_netmask_length (fun f ->
             Aws.Query.Pair ("Ipv4NetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.ipv4_ipam_pool_id (fun f ->
             Aws.Query.Pair ("Ipv4IpamPoolId", String.to_query f))
       ; Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             Aws.Query.Pair ("Ipv6CidrBlock", String.to_query f))
       ; Aws.Util.option_map v.ipv6_pool (fun f ->
             Aws.Query.Pair ("Ipv6Pool", String.to_query f))
       ; Aws.Util.option_map v.cidr_block (fun f ->
             Aws.Query.Pair ("CidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.amazon_provided_ipv6_cidr_block (fun f ->
             "amazonProvidedIpv6CidrBlock", Boolean.to_json f)
       ; Aws.Util.option_map v.instance_tenancy (fun f ->
             "instanceTenancy", Tenancy.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.vpc_encryption_control (fun f ->
             "VpcEncryptionControl", VpcEncryptionControlConfiguration.to_json f)
       ; Aws.Util.option_map v.ipv6_cidr_block_network_border_group (fun f ->
             "Ipv6CidrBlockNetworkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.ipv6_netmask_length (fun f ->
             "Ipv6NetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.ipv6_ipam_pool_id (fun f ->
             "Ipv6IpamPoolId", String.to_json f)
       ; Aws.Util.option_map v.ipv4_netmask_length (fun f ->
             "Ipv4NetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.ipv4_ipam_pool_id (fun f ->
             "Ipv4IpamPoolId", String.to_json f)
       ; Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             "Ipv6CidrBlock", String.to_json f)
       ; Aws.Util.option_map v.ipv6_pool (fun f -> "Ipv6Pool", String.to_json f)
       ; Aws.Util.option_map v.cidr_block (fun f -> "CidrBlock", String.to_json f)
       ])

let of_json j =
  { cidr_block = Aws.Util.option_map (Aws.Json.lookup j "CidrBlock") String.of_json
  ; ipv6_pool = Aws.Util.option_map (Aws.Json.lookup j "Ipv6Pool") String.of_json
  ; ipv6_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6CidrBlock") String.of_json
  ; ipv4_ipam_pool_id =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv4IpamPoolId") String.of_json
  ; ipv4_netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv4NetmaskLength") Integer.of_json
  ; ipv6_ipam_pool_id =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6IpamPoolId") String.of_json
  ; ipv6_netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6NetmaskLength") Integer.of_json
  ; ipv6_cidr_block_network_border_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "Ipv6CidrBlockNetworkBorderGroup")
        String.of_json
  ; vpc_encryption_control =
      Aws.Util.option_map
        (Aws.Json.lookup j "VpcEncryptionControl")
        VpcEncryptionControlConfiguration.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; instance_tenancy =
      Aws.Util.option_map (Aws.Json.lookup j "instanceTenancy") Tenancy.of_json
  ; amazon_provided_ipv6_cidr_block =
      Aws.Util.option_map
        (Aws.Json.lookup j "amazonProvidedIpv6CidrBlock")
        Boolean.of_json
  }
