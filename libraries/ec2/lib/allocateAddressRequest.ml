open Aws.BaseTypes

type t =
  { domain : DomainType.t option
  ; address : String.t option
  ; public_ipv4_pool : String.t option
  ; network_border_group : String.t option
  ; customer_owned_ipv4_pool : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; ipam_pool_id : String.t option
  ; dry_run : Boolean.t option
  }

let make
    ?domain
    ?address
    ?public_ipv4_pool
    ?network_border_group
    ?customer_owned_ipv4_pool
    ?(tag_specifications = [])
    ?ipam_pool_id
    ?dry_run
    () =
  { domain
  ; address
  ; public_ipv4_pool
  ; network_border_group
  ; customer_owned_ipv4_pool
  ; tag_specifications
  ; ipam_pool_id
  ; dry_run
  }

let parse xml =
  Some
    { domain = Aws.Util.option_bind (Aws.Xml.member "Domain" xml) DomainType.parse
    ; address = Aws.Util.option_bind (Aws.Xml.member "Address" xml) String.parse
    ; public_ipv4_pool =
        Aws.Util.option_bind (Aws.Xml.member "PublicIpv4Pool" xml) String.parse
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "NetworkBorderGroup" xml) String.parse
    ; customer_owned_ipv4_pool =
        Aws.Util.option_bind (Aws.Xml.member "CustomerOwnedIpv4Pool" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; ipam_pool_id = Aws.Util.option_bind (Aws.Xml.member "IpamPoolId" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.ipam_pool_id (fun f ->
             Aws.Query.Pair ("IpamPoolId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.customer_owned_ipv4_pool (fun f ->
             Aws.Query.Pair ("CustomerOwnedIpv4Pool", String.to_query f))
       ; Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.public_ipv4_pool (fun f ->
             Aws.Query.Pair ("PublicIpv4Pool", String.to_query f))
       ; Aws.Util.option_map v.address (fun f ->
             Aws.Query.Pair ("Address", String.to_query f))
       ; Aws.Util.option_map v.domain (fun f ->
             Aws.Query.Pair ("Domain", DomainType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.ipam_pool_id (fun f -> "IpamPoolId", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.customer_owned_ipv4_pool (fun f ->
             "CustomerOwnedIpv4Pool", String.to_json f)
       ; Aws.Util.option_map v.network_border_group (fun f ->
             "NetworkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.public_ipv4_pool (fun f ->
             "PublicIpv4Pool", String.to_json f)
       ; Aws.Util.option_map v.address (fun f -> "Address", String.to_json f)
       ; Aws.Util.option_map v.domain (fun f -> "Domain", DomainType.to_json f)
       ])

let of_json j =
  { domain = Aws.Util.option_map (Aws.Json.lookup j "Domain") DomainType.of_json
  ; address = Aws.Util.option_map (Aws.Json.lookup j "Address") String.of_json
  ; public_ipv4_pool =
      Aws.Util.option_map (Aws.Json.lookup j "PublicIpv4Pool") String.of_json
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkBorderGroup") String.of_json
  ; customer_owned_ipv4_pool =
      Aws.Util.option_map (Aws.Json.lookup j "CustomerOwnedIpv4Pool") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; ipam_pool_id = Aws.Util.option_map (Aws.Json.lookup j "IpamPoolId") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
