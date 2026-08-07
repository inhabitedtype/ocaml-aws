open Aws.BaseTypes

type t =
  { dns_name : String.t option
  ; fips_dns_name : String.t option
  }

let make ?dns_name ?fips_dns_name () = { dns_name; fips_dns_name }

let parse xml =
  Some
    { dns_name = Aws.Util.option_bind (Aws.Xml.member "dnsName" xml) String.parse
    ; fips_dns_name = Aws.Util.option_bind (Aws.Xml.member "fipsDnsName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fips_dns_name (fun f ->
             Aws.Query.Pair ("FipsDnsName", String.to_query f))
       ; Aws.Util.option_map v.dns_name (fun f ->
             Aws.Query.Pair ("DnsName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fips_dns_name (fun f -> "fipsDnsName", String.to_json f)
       ; Aws.Util.option_map v.dns_name (fun f -> "dnsName", String.to_json f)
       ])

let of_json j =
  { dns_name = Aws.Util.option_map (Aws.Json.lookup j "dnsName") String.of_json
  ; fips_dns_name = Aws.Util.option_map (Aws.Json.lookup j "fipsDnsName") String.of_json
  }
