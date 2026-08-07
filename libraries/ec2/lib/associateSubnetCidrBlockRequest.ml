open Aws.BaseTypes

type t =
  { ipv6_ipam_pool_id : String.t option
  ; ipv6_netmask_length : Integer.t option
  ; subnet_id : String.t
  ; ipv6_cidr_block : String.t option
  }

let make ?ipv6_ipam_pool_id ?ipv6_netmask_length ~subnet_id ?ipv6_cidr_block () =
  { ipv6_ipam_pool_id; ipv6_netmask_length; subnet_id; ipv6_cidr_block }

let parse xml =
  Some
    { ipv6_ipam_pool_id =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6IpamPoolId" xml) String.parse
    ; ipv6_netmask_length =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6NetmaskLength" xml) Integer.parse
    ; subnet_id =
        Aws.Xml.required
          "subnetId"
          (Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse)
    ; ipv6_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "ipv6CidrBlock" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             Aws.Query.Pair ("Ipv6CidrBlock", String.to_query f))
       ; Some (Aws.Query.Pair ("SubnetId", String.to_query v.subnet_id))
       ; Aws.Util.option_map v.ipv6_netmask_length (fun f ->
             Aws.Query.Pair ("Ipv6NetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.ipv6_ipam_pool_id (fun f ->
             Aws.Query.Pair ("Ipv6IpamPoolId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             "ipv6CidrBlock", String.to_json f)
       ; Some ("subnetId", String.to_json v.subnet_id)
       ; Aws.Util.option_map v.ipv6_netmask_length (fun f ->
             "Ipv6NetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.ipv6_ipam_pool_id (fun f ->
             "Ipv6IpamPoolId", String.to_json f)
       ])

let of_json j =
  { ipv6_ipam_pool_id =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6IpamPoolId") String.of_json
  ; ipv6_netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6NetmaskLength") Integer.of_json
  ; subnet_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetId"))
  ; ipv6_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6CidrBlock") String.of_json
  }
