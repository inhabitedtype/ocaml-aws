open Aws.BaseTypes

type t =
  { dns_name : String.t option
  ; hosted_zone_id : String.t option
  }

let make ?dns_name ?hosted_zone_id () = { dns_name; hosted_zone_id }

let parse xml =
  Some
    { dns_name = Aws.Util.option_bind (Aws.Xml.member "dnsName" xml) String.parse
    ; hosted_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "hostedZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hosted_zone_id (fun f ->
             Aws.Query.Pair ("HostedZoneId", String.to_query f))
       ; Aws.Util.option_map v.dns_name (fun f ->
             Aws.Query.Pair ("DnsName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hosted_zone_id (fun f -> "hostedZoneId", String.to_json f)
       ; Aws.Util.option_map v.dns_name (fun f -> "dnsName", String.to_json f)
       ])

let of_json j =
  { dns_name = Aws.Util.option_map (Aws.Json.lookup j "dnsName") String.of_json
  ; hosted_zone_id = Aws.Util.option_map (Aws.Json.lookup j "hostedZoneId") String.of_json
  }
