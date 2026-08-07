open Aws.BaseTypes

type t =
  { cidr_block : String.t option
  ; ipv6_cidr_block_network_border_group : String.t option
  ; ipv6_pool : String.t option
  ; ipv6_cidr_block : String.t option
  ; ipv4_ipam_pool_id : String.t option
  ; ipv4_netmask_length : Integer.t option
  ; ipv6_ipam_pool_id : String.t option
  ; ipv6_netmask_length : Integer.t option
  ; vpc_id : String.t
  ; amazon_provided_ipv6_cidr_block : Boolean.t option
  }

let make
    ?cidr_block
    ?ipv6_cidr_block_network_border_group
    ?ipv6_pool
    ?ipv6_cidr_block
    ?ipv4_ipam_pool_id
    ?ipv4_netmask_length
    ?ipv6_ipam_pool_id
    ?ipv6_netmask_length
    ~vpc_id
    ?amazon_provided_ipv6_cidr_block
    () =
  { cidr_block
  ; ipv6_cidr_block_network_border_group
  ; ipv6_pool
  ; ipv6_cidr_block
  ; ipv4_ipam_pool_id
  ; ipv4_netmask_length
  ; ipv6_ipam_pool_id
  ; ipv6_netmask_length
  ; vpc_id
  ; amazon_provided_ipv6_cidr_block
  }

let parse xml =
  Some
    { cidr_block = Aws.Util.option_bind (Aws.Xml.member "CidrBlock" xml) String.parse
    ; ipv6_cidr_block_network_border_group =
        Aws.Util.option_bind
          (Aws.Xml.member "Ipv6CidrBlockNetworkBorderGroup" xml)
          String.parse
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
    ; vpc_id =
        Aws.Xml.required
          "vpcId"
          (Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse)
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
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
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
       ; Aws.Util.option_map v.ipv6_cidr_block_network_border_group (fun f ->
             Aws.Query.Pair ("Ipv6CidrBlockNetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.cidr_block (fun f ->
             Aws.Query.Pair ("CidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.amazon_provided_ipv6_cidr_block (fun f ->
             "amazonProvidedIpv6CidrBlock", Boolean.to_json f)
       ; Some ("vpcId", String.to_json v.vpc_id)
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
       ; Aws.Util.option_map v.ipv6_cidr_block_network_border_group (fun f ->
             "Ipv6CidrBlockNetworkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.cidr_block (fun f -> "CidrBlock", String.to_json f)
       ])

let of_json j =
  { cidr_block = Aws.Util.option_map (Aws.Json.lookup j "CidrBlock") String.of_json
  ; ipv6_cidr_block_network_border_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "Ipv6CidrBlockNetworkBorderGroup")
        String.of_json
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
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcId"))
  ; amazon_provided_ipv6_cidr_block =
      Aws.Util.option_map
        (Aws.Json.lookup j "amazonProvidedIpv6CidrBlock")
        Boolean.of_json
  }
