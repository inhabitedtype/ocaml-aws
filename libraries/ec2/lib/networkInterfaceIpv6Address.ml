open Aws.BaseTypes

type t =
  { ipv6_address : String.t option
  ; public_ipv6_dns_name : String.t option
  ; is_primary_ipv6 : Boolean.t option
  }

let make ?ipv6_address ?public_ipv6_dns_name ?is_primary_ipv6 () =
  { ipv6_address; public_ipv6_dns_name; is_primary_ipv6 }

let parse xml =
  Some
    { ipv6_address = Aws.Util.option_bind (Aws.Xml.member "ipv6Address" xml) String.parse
    ; public_ipv6_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "publicIpv6DnsName" xml) String.parse
    ; is_primary_ipv6 =
        Aws.Util.option_bind (Aws.Xml.member "isPrimaryIpv6" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_primary_ipv6 (fun f ->
             Aws.Query.Pair ("IsPrimaryIpv6", Boolean.to_query f))
       ; Aws.Util.option_map v.public_ipv6_dns_name (fun f ->
             Aws.Query.Pair ("PublicIpv6DnsName", String.to_query f))
       ; Aws.Util.option_map v.ipv6_address (fun f ->
             Aws.Query.Pair ("Ipv6Address", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_primary_ipv6 (fun f ->
             "isPrimaryIpv6", Boolean.to_json f)
       ; Aws.Util.option_map v.public_ipv6_dns_name (fun f ->
             "publicIpv6DnsName", String.to_json f)
       ; Aws.Util.option_map v.ipv6_address (fun f -> "ipv6Address", String.to_json f)
       ])

let of_json j =
  { ipv6_address = Aws.Util.option_map (Aws.Json.lookup j "ipv6Address") String.of_json
  ; public_ipv6_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "publicIpv6DnsName") String.of_json
  ; is_primary_ipv6 =
      Aws.Util.option_map (Aws.Json.lookup j "isPrimaryIpv6") Boolean.of_json
  }
