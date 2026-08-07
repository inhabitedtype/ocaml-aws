open Aws.BaseTypes

type t =
  { association_id : String.t option
  ; branch_interface_id : String.t option
  ; trunk_interface_id : String.t option
  ; interface_protocol : InterfaceProtocolType.t option
  ; vlan_id : Integer.t option
  ; gre_key : Integer.t option
  ; tags : TagList.t
  }

let make
    ?association_id
    ?branch_interface_id
    ?trunk_interface_id
    ?interface_protocol
    ?vlan_id
    ?gre_key
    ?(tags = [])
    () =
  { association_id
  ; branch_interface_id
  ; trunk_interface_id
  ; interface_protocol
  ; vlan_id
  ; gre_key
  ; tags
  }

let parse xml =
  Some
    { association_id =
        Aws.Util.option_bind (Aws.Xml.member "associationId" xml) String.parse
    ; branch_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "branchInterfaceId" xml) String.parse
    ; trunk_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "trunkInterfaceId" xml) String.parse
    ; interface_protocol =
        Aws.Util.option_bind
          (Aws.Xml.member "interfaceProtocol" xml)
          InterfaceProtocolType.parse
    ; vlan_id = Aws.Util.option_bind (Aws.Xml.member "vlanId" xml) Integer.parse
    ; gre_key = Aws.Util.option_bind (Aws.Xml.member "greKey" xml) Integer.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.gre_key (fun f ->
             Aws.Query.Pair ("GreKey", Integer.to_query f))
       ; Aws.Util.option_map v.vlan_id (fun f ->
             Aws.Query.Pair ("VlanId", Integer.to_query f))
       ; Aws.Util.option_map v.interface_protocol (fun f ->
             Aws.Query.Pair ("InterfaceProtocol", InterfaceProtocolType.to_query f))
       ; Aws.Util.option_map v.trunk_interface_id (fun f ->
             Aws.Query.Pair ("TrunkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.branch_interface_id (fun f ->
             Aws.Query.Pair ("BranchInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.association_id (fun f ->
             Aws.Query.Pair ("AssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.gre_key (fun f -> "greKey", Integer.to_json f)
       ; Aws.Util.option_map v.vlan_id (fun f -> "vlanId", Integer.to_json f)
       ; Aws.Util.option_map v.interface_protocol (fun f ->
             "interfaceProtocol", InterfaceProtocolType.to_json f)
       ; Aws.Util.option_map v.trunk_interface_id (fun f ->
             "trunkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.branch_interface_id (fun f ->
             "branchInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.association_id (fun f -> "associationId", String.to_json f)
       ])

let of_json j =
  { association_id =
      Aws.Util.option_map (Aws.Json.lookup j "associationId") String.of_json
  ; branch_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "branchInterfaceId") String.of_json
  ; trunk_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "trunkInterfaceId") String.of_json
  ; interface_protocol =
      Aws.Util.option_map
        (Aws.Json.lookup j "interfaceProtocol")
        InterfaceProtocolType.of_json
  ; vlan_id = Aws.Util.option_map (Aws.Json.lookup j "vlanId") Integer.of_json
  ; gre_key = Aws.Util.option_map (Aws.Json.lookup j "greKey") Integer.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
