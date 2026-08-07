open Aws.BaseTypes

type t =
  { dns_record_ip_type : DnsRecordIpType.t option
  ; private_dns_only_for_inbound_resolver_endpoint : Boolean.t option
  ; private_dns_preference : String.t option
  ; private_dns_specified_domains : PrivateDnsSpecifiedDomainSet.t
  }

let make
    ?dns_record_ip_type
    ?private_dns_only_for_inbound_resolver_endpoint
    ?private_dns_preference
    ?(private_dns_specified_domains = [])
    () =
  { dns_record_ip_type
  ; private_dns_only_for_inbound_resolver_endpoint
  ; private_dns_preference
  ; private_dns_specified_domains
  }

let parse xml =
  Some
    { dns_record_ip_type =
        Aws.Util.option_bind (Aws.Xml.member "dnsRecordIpType" xml) DnsRecordIpType.parse
    ; private_dns_only_for_inbound_resolver_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "privateDnsOnlyForInboundResolverEndpoint" xml)
          Boolean.parse
    ; private_dns_preference =
        Aws.Util.option_bind (Aws.Xml.member "privateDnsPreference" xml) String.parse
    ; private_dns_specified_domains =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "privateDnsSpecifiedDomainSet" xml)
             PrivateDnsSpecifiedDomainSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PrivateDnsSpecifiedDomainSet"
              , PrivateDnsSpecifiedDomainSet.to_query v.private_dns_specified_domains ))
       ; Aws.Util.option_map v.private_dns_preference (fun f ->
             Aws.Query.Pair ("PrivateDnsPreference", String.to_query f))
       ; Aws.Util.option_map v.private_dns_only_for_inbound_resolver_endpoint (fun f ->
             Aws.Query.Pair
               ("PrivateDnsOnlyForInboundResolverEndpoint", Boolean.to_query f))
       ; Aws.Util.option_map v.dns_record_ip_type (fun f ->
             Aws.Query.Pair ("DnsRecordIpType", DnsRecordIpType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "privateDnsSpecifiedDomainSet"
           , PrivateDnsSpecifiedDomainSet.to_json v.private_dns_specified_domains )
       ; Aws.Util.option_map v.private_dns_preference (fun f ->
             "privateDnsPreference", String.to_json f)
       ; Aws.Util.option_map v.private_dns_only_for_inbound_resolver_endpoint (fun f ->
             "privateDnsOnlyForInboundResolverEndpoint", Boolean.to_json f)
       ; Aws.Util.option_map v.dns_record_ip_type (fun f ->
             "dnsRecordIpType", DnsRecordIpType.to_json f)
       ])

let of_json j =
  { dns_record_ip_type =
      Aws.Util.option_map (Aws.Json.lookup j "dnsRecordIpType") DnsRecordIpType.of_json
  ; private_dns_only_for_inbound_resolver_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "privateDnsOnlyForInboundResolverEndpoint")
        Boolean.of_json
  ; private_dns_preference =
      Aws.Util.option_map (Aws.Json.lookup j "privateDnsPreference") String.of_json
  ; private_dns_specified_domains =
      PrivateDnsSpecifiedDomainSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "privateDnsSpecifiedDomainSet"))
  }
