open Aws.BaseTypes

type t =
  { association : InstanceNetworkInterfaceAssociation.t option
  ; primary : Boolean.t option
  ; private_dns_name : String.t option
  ; private_ip_address : String.t option
  }

let make ?association ?primary ?private_dns_name ?private_ip_address () =
  { association; primary; private_dns_name; private_ip_address }

let parse xml =
  Some
    { association =
        Aws.Util.option_bind
          (Aws.Xml.member "association" xml)
          InstanceNetworkInterfaceAssociation.parse
    ; primary = Aws.Util.option_bind (Aws.Xml.member "primary" xml) Boolean.parse
    ; private_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "privateDnsName" xml) String.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddress" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             Aws.Query.Pair ("PrivateDnsName", String.to_query f))
       ; Aws.Util.option_map v.primary (fun f ->
             Aws.Query.Pair ("Primary", Boolean.to_query f))
       ; Aws.Util.option_map v.association (fun f ->
             Aws.Query.Pair ("Association", InstanceNetworkInterfaceAssociation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_ip_address (fun f ->
             "privateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             "privateDnsName", String.to_json f)
       ; Aws.Util.option_map v.primary (fun f -> "primary", Boolean.to_json f)
       ; Aws.Util.option_map v.association (fun f ->
             "association", InstanceNetworkInterfaceAssociation.to_json f)
       ])

let of_json j =
  { association =
      Aws.Util.option_map
        (Aws.Json.lookup j "association")
        InstanceNetworkInterfaceAssociation.of_json
  ; primary = Aws.Util.option_map (Aws.Json.lookup j "primary") Boolean.of_json
  ; private_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "privateDnsName") String.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddress") String.of_json
  }
