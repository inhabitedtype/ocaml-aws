open Aws.BaseTypes

type t =
  { association_id : String.t option
  ; ipv6_cidr_block : String.t option
  ; ipv6_cidr_block_state : SubnetCidrBlockState.t option
  ; ipv6_address_attribute : Ipv6AddressAttribute.t option
  ; ip_source : IpSource.t option
  }

let make
    ?association_id
    ?ipv6_cidr_block
    ?ipv6_cidr_block_state
    ?ipv6_address_attribute
    ?ip_source
    () =
  { association_id
  ; ipv6_cidr_block
  ; ipv6_cidr_block_state
  ; ipv6_address_attribute
  ; ip_source
  }

let parse xml =
  Some
    { association_id =
        Aws.Util.option_bind (Aws.Xml.member "associationId" xml) String.parse
    ; ipv6_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "ipv6CidrBlock" xml) String.parse
    ; ipv6_cidr_block_state =
        Aws.Util.option_bind
          (Aws.Xml.member "ipv6CidrBlockState" xml)
          SubnetCidrBlockState.parse
    ; ipv6_address_attribute =
        Aws.Util.option_bind
          (Aws.Xml.member "ipv6AddressAttribute" xml)
          Ipv6AddressAttribute.parse
    ; ip_source = Aws.Util.option_bind (Aws.Xml.member "ipSource" xml) IpSource.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ip_source (fun f ->
             Aws.Query.Pair ("IpSource", IpSource.to_query f))
       ; Aws.Util.option_map v.ipv6_address_attribute (fun f ->
             Aws.Query.Pair ("Ipv6AddressAttribute", Ipv6AddressAttribute.to_query f))
       ; Aws.Util.option_map v.ipv6_cidr_block_state (fun f ->
             Aws.Query.Pair ("Ipv6CidrBlockState", SubnetCidrBlockState.to_query f))
       ; Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             Aws.Query.Pair ("Ipv6CidrBlock", String.to_query f))
       ; Aws.Util.option_map v.association_id (fun f ->
             Aws.Query.Pair ("AssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ip_source (fun f -> "ipSource", IpSource.to_json f)
       ; Aws.Util.option_map v.ipv6_address_attribute (fun f ->
             "ipv6AddressAttribute", Ipv6AddressAttribute.to_json f)
       ; Aws.Util.option_map v.ipv6_cidr_block_state (fun f ->
             "ipv6CidrBlockState", SubnetCidrBlockState.to_json f)
       ; Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             "ipv6CidrBlock", String.to_json f)
       ; Aws.Util.option_map v.association_id (fun f -> "associationId", String.to_json f)
       ])

let of_json j =
  { association_id =
      Aws.Util.option_map (Aws.Json.lookup j "associationId") String.of_json
  ; ipv6_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6CidrBlock") String.of_json
  ; ipv6_cidr_block_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipv6CidrBlockState")
        SubnetCidrBlockState.of_json
  ; ipv6_address_attribute =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipv6AddressAttribute")
        Ipv6AddressAttribute.of_json
  ; ip_source = Aws.Util.option_map (Aws.Json.lookup j "ipSource") IpSource.of_json
  }
