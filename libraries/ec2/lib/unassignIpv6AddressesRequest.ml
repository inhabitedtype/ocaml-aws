open Aws.BaseTypes

type t =
  { ipv6_prefixes : IpPrefixList.t
  ; network_interface_id : String.t
  ; ipv6_addresses : Ipv6AddressList.t
  }

let make ?(ipv6_prefixes = []) ~network_interface_id ?(ipv6_addresses = []) () =
  { ipv6_prefixes; network_interface_id; ipv6_addresses }

let parse xml =
  Some
    { ipv6_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Ipv6Prefix" xml) IpPrefixList.parse)
    ; network_interface_id =
        Aws.Xml.required
          "networkInterfaceId"
          (Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse)
    ; ipv6_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6Addresses" xml)
             Ipv6AddressList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Ipv6Addresses", Ipv6AddressList.to_query v.ipv6_addresses))
       ; Some
           (Aws.Query.Pair ("NetworkInterfaceId", String.to_query v.network_interface_id))
       ; Some (Aws.Query.Pair ("Ipv6Prefix", IpPrefixList.to_query v.ipv6_prefixes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ipv6Addresses", Ipv6AddressList.to_json v.ipv6_addresses)
       ; Some ("networkInterfaceId", String.to_json v.network_interface_id)
       ; Some ("Ipv6Prefix", IpPrefixList.to_json v.ipv6_prefixes)
       ])

let of_json j =
  { ipv6_prefixes =
      IpPrefixList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Ipv6Prefix"))
  ; network_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceId"))
  ; ipv6_addresses =
      Ipv6AddressList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6Addresses"))
  }
