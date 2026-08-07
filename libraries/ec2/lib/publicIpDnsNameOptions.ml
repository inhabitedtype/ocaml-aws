open Aws.BaseTypes

type t =
  { dns_hostname_type : String.t option
  ; public_ipv4_dns_name : String.t option
  ; public_ipv6_dns_name : String.t option
  ; public_dual_stack_dns_name : String.t option
  }

let make
    ?dns_hostname_type
    ?public_ipv4_dns_name
    ?public_ipv6_dns_name
    ?public_dual_stack_dns_name
    () =
  { dns_hostname_type
  ; public_ipv4_dns_name
  ; public_ipv6_dns_name
  ; public_dual_stack_dns_name
  }

let parse xml =
  Some
    { dns_hostname_type =
        Aws.Util.option_bind (Aws.Xml.member "dnsHostnameType" xml) String.parse
    ; public_ipv4_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "publicIpv4DnsName" xml) String.parse
    ; public_ipv6_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "publicIpv6DnsName" xml) String.parse
    ; public_dual_stack_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "publicDualStackDnsName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_dual_stack_dns_name (fun f ->
             Aws.Query.Pair ("PublicDualStackDnsName", String.to_query f))
       ; Aws.Util.option_map v.public_ipv6_dns_name (fun f ->
             Aws.Query.Pair ("PublicIpv6DnsName", String.to_query f))
       ; Aws.Util.option_map v.public_ipv4_dns_name (fun f ->
             Aws.Query.Pair ("PublicIpv4DnsName", String.to_query f))
       ; Aws.Util.option_map v.dns_hostname_type (fun f ->
             Aws.Query.Pair ("DnsHostnameType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_dual_stack_dns_name (fun f ->
             "publicDualStackDnsName", String.to_json f)
       ; Aws.Util.option_map v.public_ipv6_dns_name (fun f ->
             "publicIpv6DnsName", String.to_json f)
       ; Aws.Util.option_map v.public_ipv4_dns_name (fun f ->
             "publicIpv4DnsName", String.to_json f)
       ; Aws.Util.option_map v.dns_hostname_type (fun f ->
             "dnsHostnameType", String.to_json f)
       ])

let of_json j =
  { dns_hostname_type =
      Aws.Util.option_map (Aws.Json.lookup j "dnsHostnameType") String.of_json
  ; public_ipv4_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "publicIpv4DnsName") String.of_json
  ; public_ipv6_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "publicIpv6DnsName") String.of_json
  ; public_dual_stack_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "publicDualStackDnsName") String.of_json
  }
