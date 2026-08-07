open Aws.BaseTypes

type t =
  { description : String.t option
  ; group_id : String.t option
  ; group_name : String.t option
  ; stale_ip_permissions : StaleIpPermissionSet.t
  ; stale_ip_permissions_egress : StaleIpPermissionSet.t
  ; vpc_id : String.t option
  }

let make
    ?description
    ?group_id
    ?group_name
    ?(stale_ip_permissions = [])
    ?(stale_ip_permissions_egress = [])
    ?vpc_id
    () =
  { description
  ; group_id
  ; group_name
  ; stale_ip_permissions
  ; stale_ip_permissions_egress
  ; vpc_id
  }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse
    ; stale_ip_permissions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "staleIpPermissions" xml)
             StaleIpPermissionSet.parse)
    ; stale_ip_permissions_egress =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "staleIpPermissionsEgress" xml)
             StaleIpPermissionSet.parse)
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "StaleIpPermissionsEgress"
              , StaleIpPermissionSet.to_query v.stale_ip_permissions_egress ))
       ; Some
           (Aws.Query.Pair
              ("StaleIpPermissions", StaleIpPermissionSet.to_query v.stale_ip_permissions))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Some
           ( "staleIpPermissionsEgress"
           , StaleIpPermissionSet.to_json v.stale_ip_permissions_egress )
       ; Some ("staleIpPermissions", StaleIpPermissionSet.to_json v.stale_ip_permissions)
       ; Aws.Util.option_map v.group_name (fun f -> "groupName", String.to_json f)
       ; Aws.Util.option_map v.group_id (fun f -> "groupId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "groupId") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "groupName") String.of_json
  ; stale_ip_permissions =
      StaleIpPermissionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "staleIpPermissions"))
  ; stale_ip_permissions_egress =
      StaleIpPermissionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "staleIpPermissionsEgress"))
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  }
