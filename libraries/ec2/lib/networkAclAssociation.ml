open Aws.BaseTypes

type t =
  { network_acl_association_id : String.t option
  ; network_acl_id : String.t option
  ; subnet_id : String.t option
  }

let make ?network_acl_association_id ?network_acl_id ?subnet_id () =
  { network_acl_association_id; network_acl_id; subnet_id }

let parse xml =
  Some
    { network_acl_association_id =
        Aws.Util.option_bind (Aws.Xml.member "networkAclAssociationId" xml) String.parse
    ; network_acl_id =
        Aws.Util.option_bind (Aws.Xml.member "networkAclId" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.network_acl_id (fun f ->
             Aws.Query.Pair ("NetworkAclId", String.to_query f))
       ; Aws.Util.option_map v.network_acl_association_id (fun f ->
             Aws.Query.Pair ("NetworkAclAssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.network_acl_id (fun f -> "networkAclId", String.to_json f)
       ; Aws.Util.option_map v.network_acl_association_id (fun f ->
             "networkAclAssociationId", String.to_json f)
       ])

let of_json j =
  { network_acl_association_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkAclAssociationId") String.of_json
  ; network_acl_id = Aws.Util.option_map (Aws.Json.lookup j "networkAclId") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  }
