open Aws.BaseTypes

type t =
  { allow_dns_resolution_from_remote_vpc : Boolean.t option
  ; allow_egress_from_local_classic_link_to_remote_vpc : Boolean.t option
  ; allow_egress_from_local_vpc_to_remote_classic_link : Boolean.t option
  }

let make
    ?allow_dns_resolution_from_remote_vpc
    ?allow_egress_from_local_classic_link_to_remote_vpc
    ?allow_egress_from_local_vpc_to_remote_classic_link
    () =
  { allow_dns_resolution_from_remote_vpc
  ; allow_egress_from_local_classic_link_to_remote_vpc
  ; allow_egress_from_local_vpc_to_remote_classic_link
  }

let parse xml =
  Some
    { allow_dns_resolution_from_remote_vpc =
        Aws.Util.option_bind
          (Aws.Xml.member "allowDnsResolutionFromRemoteVpc" xml)
          Boolean.parse
    ; allow_egress_from_local_classic_link_to_remote_vpc =
        Aws.Util.option_bind
          (Aws.Xml.member "allowEgressFromLocalClassicLinkToRemoteVpc" xml)
          Boolean.parse
    ; allow_egress_from_local_vpc_to_remote_classic_link =
        Aws.Util.option_bind
          (Aws.Xml.member "allowEgressFromLocalVpcToRemoteClassicLink" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map
           v.allow_egress_from_local_vpc_to_remote_classic_link
           (fun f ->
             Aws.Query.Pair
               ("AllowEgressFromLocalVpcToRemoteClassicLink", Boolean.to_query f))
       ; Aws.Util.option_map
           v.allow_egress_from_local_classic_link_to_remote_vpc
           (fun f ->
             Aws.Query.Pair
               ("AllowEgressFromLocalClassicLinkToRemoteVpc", Boolean.to_query f))
       ; Aws.Util.option_map v.allow_dns_resolution_from_remote_vpc (fun f ->
             Aws.Query.Pair ("AllowDnsResolutionFromRemoteVpc", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map
           v.allow_egress_from_local_vpc_to_remote_classic_link
           (fun f -> "allowEgressFromLocalVpcToRemoteClassicLink", Boolean.to_json f)
       ; Aws.Util.option_map
           v.allow_egress_from_local_classic_link_to_remote_vpc
           (fun f -> "allowEgressFromLocalClassicLinkToRemoteVpc", Boolean.to_json f)
       ; Aws.Util.option_map v.allow_dns_resolution_from_remote_vpc (fun f ->
             "allowDnsResolutionFromRemoteVpc", Boolean.to_json f)
       ])

let of_json j =
  { allow_dns_resolution_from_remote_vpc =
      Aws.Util.option_map
        (Aws.Json.lookup j "allowDnsResolutionFromRemoteVpc")
        Boolean.of_json
  ; allow_egress_from_local_classic_link_to_remote_vpc =
      Aws.Util.option_map
        (Aws.Json.lookup j "allowEgressFromLocalClassicLinkToRemoteVpc")
        Boolean.of_json
  ; allow_egress_from_local_vpc_to_remote_classic_link =
      Aws.Util.option_map
        (Aws.Json.lookup j "allowEgressFromLocalVpcToRemoteClassicLink")
        Boolean.of_json
  }
