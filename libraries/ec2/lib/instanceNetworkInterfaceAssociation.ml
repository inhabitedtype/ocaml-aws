open Aws.BaseTypes

type t =
  { carrier_ip : String.t option
  ; customer_owned_ip : String.t option
  ; ip_owner_id : String.t option
  ; public_dns_name : String.t option
  ; public_ip : String.t option
  }

let make ?carrier_ip ?customer_owned_ip ?ip_owner_id ?public_dns_name ?public_ip () =
  { carrier_ip; customer_owned_ip; ip_owner_id; public_dns_name; public_ip }

let parse xml =
  Some
    { carrier_ip = Aws.Util.option_bind (Aws.Xml.member "carrierIp" xml) String.parse
    ; customer_owned_ip =
        Aws.Util.option_bind (Aws.Xml.member "customerOwnedIp" xml) String.parse
    ; ip_owner_id = Aws.Util.option_bind (Aws.Xml.member "ipOwnerId" xml) String.parse
    ; public_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "publicDnsName" xml) String.parse
    ; public_ip = Aws.Util.option_bind (Aws.Xml.member "publicIp" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ; Aws.Util.option_map v.public_dns_name (fun f ->
             Aws.Query.Pair ("PublicDnsName", String.to_query f))
       ; Aws.Util.option_map v.ip_owner_id (fun f ->
             Aws.Query.Pair ("IpOwnerId", String.to_query f))
       ; Aws.Util.option_map v.customer_owned_ip (fun f ->
             Aws.Query.Pair ("CustomerOwnedIp", String.to_query f))
       ; Aws.Util.option_map v.carrier_ip (fun f ->
             Aws.Query.Pair ("CarrierIp", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_ip (fun f -> "publicIp", String.to_json f)
       ; Aws.Util.option_map v.public_dns_name (fun f ->
             "publicDnsName", String.to_json f)
       ; Aws.Util.option_map v.ip_owner_id (fun f -> "ipOwnerId", String.to_json f)
       ; Aws.Util.option_map v.customer_owned_ip (fun f ->
             "customerOwnedIp", String.to_json f)
       ; Aws.Util.option_map v.carrier_ip (fun f -> "carrierIp", String.to_json f)
       ])

let of_json j =
  { carrier_ip = Aws.Util.option_map (Aws.Json.lookup j "carrierIp") String.of_json
  ; customer_owned_ip =
      Aws.Util.option_map (Aws.Json.lookup j "customerOwnedIp") String.of_json
  ; ip_owner_id = Aws.Util.option_map (Aws.Json.lookup j "ipOwnerId") String.of_json
  ; public_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "publicDnsName") String.of_json
  ; public_ip = Aws.Util.option_map (Aws.Json.lookup j "publicIp") String.of_json
  }
