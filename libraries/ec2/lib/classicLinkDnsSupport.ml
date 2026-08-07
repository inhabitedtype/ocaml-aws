open Aws.BaseTypes

type t =
  { classic_link_dns_supported : Boolean.t option
  ; vpc_id : String.t option
  }

let make ?classic_link_dns_supported ?vpc_id () = { classic_link_dns_supported; vpc_id }

let parse xml =
  Some
    { classic_link_dns_supported =
        Aws.Util.option_bind (Aws.Xml.member "classicLinkDnsSupported" xml) Boolean.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.classic_link_dns_supported (fun f ->
             Aws.Query.Pair ("ClassicLinkDnsSupported", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.classic_link_dns_supported (fun f ->
             "classicLinkDnsSupported", Boolean.to_json f)
       ])

let of_json j =
  { classic_link_dns_supported =
      Aws.Util.option_map (Aws.Json.lookup j "classicLinkDnsSupported") Boolean.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  }
