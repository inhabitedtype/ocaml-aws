open Aws.BaseTypes

type t =
  { allocation_id : String.t option
  ; association_id : String.t option
  ; domain : DomainType.t option
  ; network_interface_id : String.t option
  ; network_interface_owner_id : String.t option
  ; private_ip_address : String.t option
  ; tags : TagList.t
  ; public_ipv4_pool : String.t option
  ; network_border_group : String.t option
  ; customer_owned_ip : String.t option
  ; customer_owned_ipv4_pool : String.t option
  ; carrier_ip : String.t option
  ; subnet_id : String.t option
  ; service_managed : ServiceManaged.t option
  ; instance_id : String.t option
  ; public_ip : String.t option
  }

let make
    ?allocation_id
    ?association_id
    ?domain
    ?network_interface_id
    ?network_interface_owner_id
    ?private_ip_address
    ?(tags = [])
    ?public_ipv4_pool
    ?network_border_group
    ?customer_owned_ip
    ?customer_owned_ipv4_pool
    ?carrier_ip
    ?subnet_id
    ?service_managed
    ?instance_id
    ?public_ip
    () =
  { allocation_id
  ; association_id
  ; domain
  ; network_interface_id
  ; network_interface_owner_id
  ; private_ip_address
  ; tags
  ; public_ipv4_pool
  ; network_border_group
  ; customer_owned_ip
  ; customer_owned_ipv4_pool
  ; carrier_ip
  ; subnet_id
  ; service_managed
  ; instance_id
  ; public_ip
  }

let parse xml =
  Some
    { allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "allocationId" xml) String.parse
    ; association_id =
        Aws.Util.option_bind (Aws.Xml.member "associationId" xml) String.parse
    ; domain = Aws.Util.option_bind (Aws.Xml.member "domain" xml) DomainType.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; network_interface_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceOwnerId" xml) String.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddress" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; public_ipv4_pool =
        Aws.Util.option_bind (Aws.Xml.member "publicIpv4Pool" xml) String.parse
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "networkBorderGroup" xml) String.parse
    ; customer_owned_ip =
        Aws.Util.option_bind (Aws.Xml.member "customerOwnedIp" xml) String.parse
    ; customer_owned_ipv4_pool =
        Aws.Util.option_bind (Aws.Xml.member "customerOwnedIpv4Pool" xml) String.parse
    ; carrier_ip = Aws.Util.option_bind (Aws.Xml.member "carrierIp" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; service_managed =
        Aws.Util.option_bind (Aws.Xml.member "serviceManaged" xml) ServiceManaged.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; public_ip = Aws.Util.option_bind (Aws.Xml.member "publicIp" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.service_managed (fun f ->
             Aws.Query.Pair ("ServiceManaged", ServiceManaged.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.carrier_ip (fun f ->
             Aws.Query.Pair ("CarrierIp", String.to_query f))
       ; Aws.Util.option_map v.customer_owned_ipv4_pool (fun f ->
             Aws.Query.Pair ("CustomerOwnedIpv4Pool", String.to_query f))
       ; Aws.Util.option_map v.customer_owned_ip (fun f ->
             Aws.Query.Pair ("CustomerOwnedIp", String.to_query f))
       ; Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.public_ipv4_pool (fun f ->
             Aws.Query.Pair ("PublicIpv4Pool", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.network_interface_owner_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceOwnerId", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.domain (fun f ->
             Aws.Query.Pair ("Domain", DomainType.to_query f))
       ; Aws.Util.option_map v.association_id (fun f ->
             Aws.Query.Pair ("AssociationId", String.to_query f))
       ; Aws.Util.option_map v.allocation_id (fun f ->
             Aws.Query.Pair ("AllocationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_ip (fun f -> "publicIp", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.service_managed (fun f ->
             "serviceManaged", ServiceManaged.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.carrier_ip (fun f -> "carrierIp", String.to_json f)
       ; Aws.Util.option_map v.customer_owned_ipv4_pool (fun f ->
             "customerOwnedIpv4Pool", String.to_json f)
       ; Aws.Util.option_map v.customer_owned_ip (fun f ->
             "customerOwnedIp", String.to_json f)
       ; Aws.Util.option_map v.network_border_group (fun f ->
             "networkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.public_ipv4_pool (fun f ->
             "publicIpv4Pool", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             "privateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.network_interface_owner_id (fun f ->
             "networkInterfaceOwnerId", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.domain (fun f -> "domain", DomainType.to_json f)
       ; Aws.Util.option_map v.association_id (fun f -> "associationId", String.to_json f)
       ; Aws.Util.option_map v.allocation_id (fun f -> "allocationId", String.to_json f)
       ])

let of_json j =
  { allocation_id = Aws.Util.option_map (Aws.Json.lookup j "allocationId") String.of_json
  ; association_id =
      Aws.Util.option_map (Aws.Json.lookup j "associationId") String.of_json
  ; domain = Aws.Util.option_map (Aws.Json.lookup j "domain") DomainType.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; network_interface_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceOwnerId") String.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddress") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; public_ipv4_pool =
      Aws.Util.option_map (Aws.Json.lookup j "publicIpv4Pool") String.of_json
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "networkBorderGroup") String.of_json
  ; customer_owned_ip =
      Aws.Util.option_map (Aws.Json.lookup j "customerOwnedIp") String.of_json
  ; customer_owned_ipv4_pool =
      Aws.Util.option_map (Aws.Json.lookup j "customerOwnedIpv4Pool") String.of_json
  ; carrier_ip = Aws.Util.option_map (Aws.Json.lookup j "carrierIp") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; service_managed =
      Aws.Util.option_map (Aws.Json.lookup j "serviceManaged") ServiceManaged.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; public_ip = Aws.Util.option_map (Aws.Json.lookup j "publicIp") String.of_json
  }
