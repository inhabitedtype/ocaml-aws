open Aws.BaseTypes

type t =
  { branch_interface_id : String.t
  ; trunk_interface_id : String.t
  ; vlan_id : Integer.t option
  ; gre_key : Integer.t option
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  }

let make
    ~branch_interface_id
    ~trunk_interface_id
    ?vlan_id
    ?gre_key
    ?client_token
    ?dry_run
    () =
  { branch_interface_id; trunk_interface_id; vlan_id; gre_key; client_token; dry_run }

let parse xml =
  Some
    { branch_interface_id =
        Aws.Xml.required
          "BranchInterfaceId"
          (Aws.Util.option_bind (Aws.Xml.member "BranchInterfaceId" xml) String.parse)
    ; trunk_interface_id =
        Aws.Xml.required
          "TrunkInterfaceId"
          (Aws.Util.option_bind (Aws.Xml.member "TrunkInterfaceId" xml) String.parse)
    ; vlan_id = Aws.Util.option_bind (Aws.Xml.member "VlanId" xml) Integer.parse
    ; gre_key = Aws.Util.option_bind (Aws.Xml.member "GreKey" xml) Integer.parse
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
       ; Aws.Util.option_map v.gre_key (fun f ->
             Aws.Query.Pair ("GreKey", Integer.to_query f))
       ; Aws.Util.option_map v.vlan_id (fun f ->
             Aws.Query.Pair ("VlanId", Integer.to_query f))
       ; Some (Aws.Query.Pair ("TrunkInterfaceId", String.to_query v.trunk_interface_id))
       ; Some
           (Aws.Query.Pair ("BranchInterfaceId", String.to_query v.branch_interface_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.gre_key (fun f -> "GreKey", Integer.to_json f)
       ; Aws.Util.option_map v.vlan_id (fun f -> "VlanId", Integer.to_json f)
       ; Some ("TrunkInterfaceId", String.to_json v.trunk_interface_id)
       ; Some ("BranchInterfaceId", String.to_json v.branch_interface_id)
       ])

let of_json j =
  { branch_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "BranchInterfaceId"))
  ; trunk_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrunkInterfaceId"))
  ; vlan_id = Aws.Util.option_map (Aws.Json.lookup j "VlanId") Integer.of_json
  ; gre_key = Aws.Util.option_map (Aws.Json.lookup j "GreKey") Integer.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
