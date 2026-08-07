open Aws.BaseTypes

type t =
  { verified_access_endpoint_id : String.t
  ; verified_access_group_id : String.t option
  ; load_balancer_options : ModifyVerifiedAccessEndpointLoadBalancerOptions.t option
  ; network_interface_options : ModifyVerifiedAccessEndpointEniOptions.t option
  ; description : String.t option
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; rds_options : ModifyVerifiedAccessEndpointRdsOptions.t option
  ; cidr_options : ModifyVerifiedAccessEndpointCidrOptions.t option
  }

let make
    ~verified_access_endpoint_id
    ?verified_access_group_id
    ?load_balancer_options
    ?network_interface_options
    ?description
    ?client_token
    ?dry_run
    ?rds_options
    ?cidr_options
    () =
  { verified_access_endpoint_id
  ; verified_access_group_id
  ; load_balancer_options
  ; network_interface_options
  ; description
  ; client_token
  ; dry_run
  ; rds_options
  ; cidr_options
  }

let parse xml =
  Some
    { verified_access_endpoint_id =
        Aws.Xml.required
          "VerifiedAccessEndpointId"
          (Aws.Util.option_bind
             (Aws.Xml.member "VerifiedAccessEndpointId" xml)
             String.parse)
    ; verified_access_group_id =
        Aws.Util.option_bind (Aws.Xml.member "VerifiedAccessGroupId" xml) String.parse
    ; load_balancer_options =
        Aws.Util.option_bind
          (Aws.Xml.member "LoadBalancerOptions" xml)
          ModifyVerifiedAccessEndpointLoadBalancerOptions.parse
    ; network_interface_options =
        Aws.Util.option_bind
          (Aws.Xml.member "NetworkInterfaceOptions" xml)
          ModifyVerifiedAccessEndpointEniOptions.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; rds_options =
        Aws.Util.option_bind
          (Aws.Xml.member "RdsOptions" xml)
          ModifyVerifiedAccessEndpointRdsOptions.parse
    ; cidr_options =
        Aws.Util.option_bind
          (Aws.Xml.member "CidrOptions" xml)
          ModifyVerifiedAccessEndpointCidrOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_options (fun f ->
             Aws.Query.Pair
               ("CidrOptions", ModifyVerifiedAccessEndpointCidrOptions.to_query f))
       ; Aws.Util.option_map v.rds_options (fun f ->
             Aws.Query.Pair
               ("RdsOptions", ModifyVerifiedAccessEndpointRdsOptions.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.network_interface_options (fun f ->
             Aws.Query.Pair
               ( "NetworkInterfaceOptions"
               , ModifyVerifiedAccessEndpointEniOptions.to_query f ))
       ; Aws.Util.option_map v.load_balancer_options (fun f ->
             Aws.Query.Pair
               ( "LoadBalancerOptions"
               , ModifyVerifiedAccessEndpointLoadBalancerOptions.to_query f ))
       ; Aws.Util.option_map v.verified_access_group_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessGroupId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VerifiedAccessEndpointId", String.to_query v.verified_access_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_options (fun f ->
             "CidrOptions", ModifyVerifiedAccessEndpointCidrOptions.to_json f)
       ; Aws.Util.option_map v.rds_options (fun f ->
             "RdsOptions", ModifyVerifiedAccessEndpointRdsOptions.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.network_interface_options (fun f ->
             "NetworkInterfaceOptions", ModifyVerifiedAccessEndpointEniOptions.to_json f)
       ; Aws.Util.option_map v.load_balancer_options (fun f ->
             ( "LoadBalancerOptions"
             , ModifyVerifiedAccessEndpointLoadBalancerOptions.to_json f ))
       ; Aws.Util.option_map v.verified_access_group_id (fun f ->
             "VerifiedAccessGroupId", String.to_json f)
       ; Some ("VerifiedAccessEndpointId", String.to_json v.verified_access_endpoint_id)
       ])

let of_json j =
  { verified_access_endpoint_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VerifiedAccessEndpointId"))
  ; verified_access_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "VerifiedAccessGroupId") String.of_json
  ; load_balancer_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "LoadBalancerOptions")
        ModifyVerifiedAccessEndpointLoadBalancerOptions.of_json
  ; network_interface_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "NetworkInterfaceOptions")
        ModifyVerifiedAccessEndpointEniOptions.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; rds_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "RdsOptions")
        ModifyVerifiedAccessEndpointRdsOptions.of_json
  ; cidr_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "CidrOptions")
        ModifyVerifiedAccessEndpointCidrOptions.of_json
  }
