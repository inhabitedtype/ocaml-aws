open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t
  ; target_vpc_subnet_id : String.t option
  ; destination_cidr_block : String.t
  ; dry_run : Boolean.t option
  }

let make ~client_vpn_endpoint_id ?target_vpc_subnet_id ~destination_cidr_block ?dry_run ()
    =
  { client_vpn_endpoint_id; target_vpc_subnet_id; destination_cidr_block; dry_run }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Xml.required
          "ClientVpnEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "ClientVpnEndpointId" xml) String.parse)
    ; target_vpc_subnet_id =
        Aws.Util.option_bind (Aws.Xml.member "TargetVpcSubnetId" xml) String.parse
    ; destination_cidr_block =
        Aws.Xml.required
          "DestinationCidrBlock"
          (Aws.Util.option_bind (Aws.Xml.member "DestinationCidrBlock" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DestinationCidrBlock", String.to_query v.destination_cidr_block))
       ; Aws.Util.option_map v.target_vpc_subnet_id (fun f ->
             Aws.Query.Pair ("TargetVpcSubnetId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ClientVpnEndpointId", String.to_query v.client_vpn_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("DestinationCidrBlock", String.to_json v.destination_cidr_block)
       ; Aws.Util.option_map v.target_vpc_subnet_id (fun f ->
             "TargetVpcSubnetId", String.to_json f)
       ; Some ("ClientVpnEndpointId", String.to_json v.client_vpn_endpoint_id)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientVpnEndpointId"))
  ; target_vpc_subnet_id =
      Aws.Util.option_map (Aws.Json.lookup j "TargetVpcSubnetId") String.of_json
  ; destination_cidr_block =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DestinationCidrBlock"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
