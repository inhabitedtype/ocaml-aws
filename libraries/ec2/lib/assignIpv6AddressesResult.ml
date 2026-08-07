open Aws.BaseTypes

type t =
  { assigned_ipv6_addresses : Ipv6AddressList.t
  ; assigned_ipv6_prefixes : IpPrefixList.t
  ; network_interface_id : String.t option
  }

let make
    ?(assigned_ipv6_addresses = [])
    ?(assigned_ipv6_prefixes = [])
    ?network_interface_id
    () =
  { assigned_ipv6_addresses; assigned_ipv6_prefixes; network_interface_id }

let parse xml =
  Some
    { assigned_ipv6_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "assignedIpv6Addresses" xml)
             Ipv6AddressList.parse)
    ; assigned_ipv6_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "assignedIpv6PrefixSet" xml)
             IpPrefixList.parse)
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AssignedIpv6PrefixSet", IpPrefixList.to_query v.assigned_ipv6_prefixes))
       ; Some
           (Aws.Query.Pair
              ("AssignedIpv6Addresses", Ipv6AddressList.to_query v.assigned_ipv6_addresses))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Some ("assignedIpv6PrefixSet", IpPrefixList.to_json v.assigned_ipv6_prefixes)
       ; Some ("assignedIpv6Addresses", Ipv6AddressList.to_json v.assigned_ipv6_addresses)
       ])

let of_json j =
  { assigned_ipv6_addresses =
      Ipv6AddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "assignedIpv6Addresses"))
  ; assigned_ipv6_prefixes =
      IpPrefixList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "assignedIpv6PrefixSet"))
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  }
