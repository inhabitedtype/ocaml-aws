open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t
  ; destination_cidr_block : String.t
  ; target_vpc_subnet_id : String.t option
  ; description : String.t option
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  }

let make
    ~client_vpn_endpoint_id
    ~destination_cidr_block
    ?target_vpc_subnet_id
    ?description
    ?client_token
    ?dry_run
    () =
  { client_vpn_endpoint_id
  ; destination_cidr_block
  ; target_vpc_subnet_id
  ; description
  ; client_token
  ; dry_run
  }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Xml.required
          "ClientVpnEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "ClientVpnEndpointId" xml) String.parse)
    ; destination_cidr_block =
        Aws.Xml.required
          "DestinationCidrBlock"
          (Aws.Util.option_bind (Aws.Xml.member "DestinationCidrBlock" xml) String.parse)
    ; target_vpc_subnet_id =
        Aws.Util.option_bind (Aws.Xml.member "TargetVpcSubnetId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.target_vpc_subnet_id (fun f ->
             Aws.Query.Pair ("TargetVpcSubnetId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DestinationCidrBlock", String.to_query v.destination_cidr_block))
       ; Some
           (Aws.Query.Pair
              ("ClientVpnEndpointId", String.to_query v.client_vpn_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.target_vpc_subnet_id (fun f ->
             "TargetVpcSubnetId", String.to_json f)
       ; Some ("DestinationCidrBlock", String.to_json v.destination_cidr_block)
       ; Some ("ClientVpnEndpointId", String.to_json v.client_vpn_endpoint_id)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientVpnEndpointId"))
  ; destination_cidr_block =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DestinationCidrBlock"))
  ; target_vpc_subnet_id =
      Aws.Util.option_map (Aws.Json.lookup j "TargetVpcSubnetId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
