open Aws.BaseTypes

type t =
  { nat_gateway_id : String.t
  ; private_ip_addresses : IpList.t
  ; private_ip_address_count : Integer.t option
  ; dry_run : Boolean.t option
  }

let make
    ~nat_gateway_id
    ?(private_ip_addresses = [])
    ?private_ip_address_count
    ?dry_run
    () =
  { nat_gateway_id; private_ip_addresses; private_ip_address_count; dry_run }

let parse xml =
  Some
    { nat_gateway_id =
        Aws.Xml.required
          "NatGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "NatGatewayId" xml) String.parse)
    ; private_ip_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "PrivateIpAddress" xml) IpList.parse)
    ; private_ip_address_count =
        Aws.Util.option_bind (Aws.Xml.member "PrivateIpAddressCount" xml) Integer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.private_ip_address_count (fun f ->
             Aws.Query.Pair ("PrivateIpAddressCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("PrivateIpAddress", IpList.to_query v.private_ip_addresses))
       ; Some (Aws.Query.Pair ("NatGatewayId", String.to_query v.nat_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.private_ip_address_count (fun f ->
             "PrivateIpAddressCount", Integer.to_json f)
       ; Some ("PrivateIpAddress", IpList.to_json v.private_ip_addresses)
       ; Some ("NatGatewayId", String.to_json v.nat_gateway_id)
       ])

let of_json j =
  { nat_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NatGatewayId"))
  ; private_ip_addresses =
      IpList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrivateIpAddress"))
  ; private_ip_address_count =
      Aws.Util.option_map (Aws.Json.lookup j "PrivateIpAddressCount") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
