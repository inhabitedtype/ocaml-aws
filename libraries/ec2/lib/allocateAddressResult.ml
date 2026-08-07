open Aws.BaseTypes

type t =
  { allocation_id : String.t option
  ; public_ipv4_pool : String.t option
  ; network_border_group : String.t option
  ; domain : DomainType.t option
  ; customer_owned_ip : String.t option
  ; customer_owned_ipv4_pool : String.t option
  ; carrier_ip : String.t option
  ; public_ip : String.t option
  }

let make
    ?allocation_id
    ?public_ipv4_pool
    ?network_border_group
    ?domain
    ?customer_owned_ip
    ?customer_owned_ipv4_pool
    ?carrier_ip
    ?public_ip
    () =
  { allocation_id
  ; public_ipv4_pool
  ; network_border_group
  ; domain
  ; customer_owned_ip
  ; customer_owned_ipv4_pool
  ; carrier_ip
  ; public_ip
  }

let parse xml =
  Some
    { allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "allocationId" xml) String.parse
    ; public_ipv4_pool =
        Aws.Util.option_bind (Aws.Xml.member "publicIpv4Pool" xml) String.parse
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "networkBorderGroup" xml) String.parse
    ; domain = Aws.Util.option_bind (Aws.Xml.member "domain" xml) DomainType.parse
    ; customer_owned_ip =
        Aws.Util.option_bind (Aws.Xml.member "customerOwnedIp" xml) String.parse
    ; customer_owned_ipv4_pool =
        Aws.Util.option_bind (Aws.Xml.member "customerOwnedIpv4Pool" xml) String.parse
    ; carrier_ip = Aws.Util.option_bind (Aws.Xml.member "carrierIp" xml) String.parse
    ; public_ip = Aws.Util.option_bind (Aws.Xml.member "publicIp" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ; Aws.Util.option_map v.carrier_ip (fun f ->
             Aws.Query.Pair ("CarrierIp", String.to_query f))
       ; Aws.Util.option_map v.customer_owned_ipv4_pool (fun f ->
             Aws.Query.Pair ("CustomerOwnedIpv4Pool", String.to_query f))
       ; Aws.Util.option_map v.customer_owned_ip (fun f ->
             Aws.Query.Pair ("CustomerOwnedIp", String.to_query f))
       ; Aws.Util.option_map v.domain (fun f ->
             Aws.Query.Pair ("Domain", DomainType.to_query f))
       ; Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.public_ipv4_pool (fun f ->
             Aws.Query.Pair ("PublicIpv4Pool", String.to_query f))
       ; Aws.Util.option_map v.allocation_id (fun f ->
             Aws.Query.Pair ("AllocationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_ip (fun f -> "publicIp", String.to_json f)
       ; Aws.Util.option_map v.carrier_ip (fun f -> "carrierIp", String.to_json f)
       ; Aws.Util.option_map v.customer_owned_ipv4_pool (fun f ->
             "customerOwnedIpv4Pool", String.to_json f)
       ; Aws.Util.option_map v.customer_owned_ip (fun f ->
             "customerOwnedIp", String.to_json f)
       ; Aws.Util.option_map v.domain (fun f -> "domain", DomainType.to_json f)
       ; Aws.Util.option_map v.network_border_group (fun f ->
             "networkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.public_ipv4_pool (fun f ->
             "publicIpv4Pool", String.to_json f)
       ; Aws.Util.option_map v.allocation_id (fun f -> "allocationId", String.to_json f)
       ])

let of_json j =
  { allocation_id = Aws.Util.option_map (Aws.Json.lookup j "allocationId") String.of_json
  ; public_ipv4_pool =
      Aws.Util.option_map (Aws.Json.lookup j "publicIpv4Pool") String.of_json
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "networkBorderGroup") String.of_json
  ; domain = Aws.Util.option_map (Aws.Json.lookup j "domain") DomainType.of_json
  ; customer_owned_ip =
      Aws.Util.option_map (Aws.Json.lookup j "customerOwnedIp") String.of_json
  ; customer_owned_ipv4_pool =
      Aws.Util.option_map (Aws.Json.lookup j "customerOwnedIpv4Pool") String.of_json
  ; carrier_ip = Aws.Util.option_map (Aws.Json.lookup j "carrierIp") String.of_json
  ; public_ip = Aws.Util.option_map (Aws.Json.lookup j "publicIp") String.of_json
  }
