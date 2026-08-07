open Aws.BaseTypes

type t =
  { ipv6_prefix_count : Integer.t option
  ; ipv6_prefixes : IpPrefixList.t
  ; network_interface_id : String.t
  ; ipv6_addresses : Ipv6AddressList.t
  ; ipv6_address_count : Integer.t option
  }

let make
    ?ipv6_prefix_count
    ?(ipv6_prefixes = [])
    ~network_interface_id
    ?(ipv6_addresses = [])
    ?ipv6_address_count
    () =
  { ipv6_prefix_count
  ; ipv6_prefixes
  ; network_interface_id
  ; ipv6_addresses
  ; ipv6_address_count
  }

let parse xml =
  Some
    { ipv6_prefix_count =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6PrefixCount" xml) Integer.parse
    ; ipv6_prefixes =
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
    ; ipv6_address_count =
        Aws.Util.option_bind (Aws.Xml.member "ipv6AddressCount" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6_address_count (fun f ->
             Aws.Query.Pair ("Ipv6AddressCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("Ipv6Addresses", Ipv6AddressList.to_query v.ipv6_addresses))
       ; Some
           (Aws.Query.Pair ("NetworkInterfaceId", String.to_query v.network_interface_id))
       ; Some (Aws.Query.Pair ("Ipv6Prefix", IpPrefixList.to_query v.ipv6_prefixes))
       ; Aws.Util.option_map v.ipv6_prefix_count (fun f ->
             Aws.Query.Pair ("Ipv6PrefixCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6_address_count (fun f ->
             "ipv6AddressCount", Integer.to_json f)
       ; Some ("ipv6Addresses", Ipv6AddressList.to_json v.ipv6_addresses)
       ; Some ("networkInterfaceId", String.to_json v.network_interface_id)
       ; Some ("Ipv6Prefix", IpPrefixList.to_json v.ipv6_prefixes)
       ; Aws.Util.option_map v.ipv6_prefix_count (fun f ->
             "Ipv6PrefixCount", Integer.to_json f)
       ])

let of_json j =
  { ipv6_prefix_count =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6PrefixCount") Integer.of_json
  ; ipv6_prefixes =
      IpPrefixList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Ipv6Prefix"))
  ; network_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceId"))
  ; ipv6_addresses =
      Ipv6AddressList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6Addresses"))
  ; ipv6_address_count =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6AddressCount") Integer.of_json
  }
