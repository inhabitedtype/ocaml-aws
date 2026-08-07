open Aws.BaseTypes

type t =
  { allocation_id : String.t option
  ; instance_id : String.t option
  ; public_ip : String.t option
  ; dry_run : Boolean.t option
  ; network_interface_id : String.t option
  ; private_ip_address : String.t option
  ; allow_reassociation : Boolean.t option
  }

let make
    ?allocation_id
    ?instance_id
    ?public_ip
    ?dry_run
    ?network_interface_id
    ?private_ip_address
    ?allow_reassociation
    () =
  { allocation_id
  ; instance_id
  ; public_ip
  ; dry_run
  ; network_interface_id
  ; private_ip_address
  ; allow_reassociation
  }

let parse xml =
  Some
    { allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "AllocationId" xml) String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
    ; public_ip = Aws.Util.option_bind (Aws.Xml.member "PublicIp" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddress" xml) String.parse
    ; allow_reassociation =
        Aws.Util.option_bind (Aws.Xml.member "allowReassociation" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allow_reassociation (fun f ->
             Aws.Query.Pair ("AllowReassociation", Boolean.to_query f))
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.allocation_id (fun f ->
             Aws.Query.Pair ("AllocationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allow_reassociation (fun f ->
             "allowReassociation", Boolean.to_json f)
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             "privateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.public_ip (fun f -> "PublicIp", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f)
       ; Aws.Util.option_map v.allocation_id (fun f -> "AllocationId", String.to_json f)
       ])

let of_json j =
  { allocation_id = Aws.Util.option_map (Aws.Json.lookup j "AllocationId") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json
  ; public_ip = Aws.Util.option_map (Aws.Json.lookup j "PublicIp") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddress") String.of_json
  ; allow_reassociation =
      Aws.Util.option_map (Aws.Json.lookup j "allowReassociation") Boolean.of_json
  }
