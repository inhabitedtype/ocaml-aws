open Aws.BaseTypes

type t =
  { description : String.t option
  ; cidr_ipv6 : String.t option
  }

let make ?description ?cidr_ipv6 () = { description; cidr_ipv6 }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; cidr_ipv6 = Aws.Util.option_bind (Aws.Xml.member "cidrIpv6" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_ipv6 (fun f ->
             Aws.Query.Pair ("CidrIpv6", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_ipv6 (fun f -> "cidrIpv6", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; cidr_ipv6 = Aws.Util.option_map (Aws.Json.lookup j "cidrIpv6") String.of_json
  }
