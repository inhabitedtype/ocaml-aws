open Aws.BaseTypes

type t =
  { associations : NetworkAclAssociationList.t
  ; entries : NetworkAclEntryList.t
  ; is_default : Boolean.t option
  ; network_acl_id : String.t option
  ; tags : TagList.t
  ; vpc_id : String.t option
  ; owner_id : String.t option
  }

let make
    ?(associations = [])
    ?(entries = [])
    ?is_default
    ?network_acl_id
    ?(tags = [])
    ?vpc_id
    ?owner_id
    () =
  { associations; entries; is_default; network_acl_id; tags; vpc_id; owner_id }

let parse xml =
  Some
    { associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "associationSet" xml)
             NetworkAclAssociationList.parse)
    ; entries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "entrySet" xml) NetworkAclEntryList.parse)
    ; is_default = Aws.Util.option_bind (Aws.Xml.member "default" xml) Boolean.parse
    ; network_acl_id =
        Aws.Util.option_bind (Aws.Xml.member "networkAclId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.network_acl_id (fun f ->
             Aws.Query.Pair ("NetworkAclId", String.to_query f))
       ; Aws.Util.option_map v.is_default (fun f ->
             Aws.Query.Pair ("Default", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("EntrySet", NetworkAclEntryList.to_query v.entries))
       ; Some
           (Aws.Query.Pair
              ("AssociationSet", NetworkAclAssociationList.to_query v.associations))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.network_acl_id (fun f -> "networkAclId", String.to_json f)
       ; Aws.Util.option_map v.is_default (fun f -> "default", Boolean.to_json f)
       ; Some ("entrySet", NetworkAclEntryList.to_json v.entries)
       ; Some ("associationSet", NetworkAclAssociationList.to_json v.associations)
       ])

let of_json j =
  { associations =
      NetworkAclAssociationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "associationSet"))
  ; entries =
      NetworkAclEntryList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "entrySet"))
  ; is_default = Aws.Util.option_map (Aws.Json.lookup j "default") Boolean.of_json
  ; network_acl_id = Aws.Util.option_map (Aws.Json.lookup j "networkAclId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  }
