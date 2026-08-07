open Aws.BaseTypes

type t =
  { nat_gateway_id : String.t
  ; private_ip_addresses : IpList.t
  ; max_drain_duration_seconds : Integer.t option
  ; dry_run : Boolean.t option
  }

let make ~nat_gateway_id ~private_ip_addresses ?max_drain_duration_seconds ?dry_run () =
  { nat_gateway_id; private_ip_addresses; max_drain_duration_seconds; dry_run }

let parse xml =
  Some
    { nat_gateway_id =
        Aws.Xml.required
          "NatGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "NatGatewayId" xml) String.parse)
    ; private_ip_addresses =
        Aws.Xml.required
          "PrivateIpAddress"
          (Aws.Util.option_bind (Aws.Xml.member "PrivateIpAddress" xml) IpList.parse)
    ; max_drain_duration_seconds =
        Aws.Util.option_bind (Aws.Xml.member "MaxDrainDurationSeconds" xml) Integer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.max_drain_duration_seconds (fun f ->
             Aws.Query.Pair ("MaxDrainDurationSeconds", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("PrivateIpAddress", IpList.to_query v.private_ip_addresses))
       ; Some (Aws.Query.Pair ("NatGatewayId", String.to_query v.nat_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.max_drain_duration_seconds (fun f ->
             "MaxDrainDurationSeconds", Integer.to_json f)
       ; Some ("PrivateIpAddress", IpList.to_json v.private_ip_addresses)
       ; Some ("NatGatewayId", String.to_json v.nat_gateway_id)
       ])

let of_json j =
  { nat_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NatGatewayId"))
  ; private_ip_addresses =
      IpList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrivateIpAddress"))
  ; max_drain_duration_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "MaxDrainDurationSeconds") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
