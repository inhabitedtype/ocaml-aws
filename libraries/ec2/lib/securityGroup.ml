open Aws.BaseTypes

type t =
  { group_id : String.t
  ; ip_permissions_egress : IpPermissionList.t
  ; tags : TagList.t
  ; vpc_id : String.t option
  ; security_group_arn : String.t option
  ; owner_id : String.t
  ; group_name : String.t
  ; description : String.t
  ; ip_permissions : IpPermissionList.t
  }

let make
    ~group_id
    ?(ip_permissions_egress = [])
    ?(tags = [])
    ?vpc_id
    ?security_group_arn
    ~owner_id
    ~group_name
    ~description
    ?(ip_permissions = [])
    () =
  { group_id
  ; ip_permissions_egress
  ; tags
  ; vpc_id
  ; security_group_arn
  ; owner_id
  ; group_name
  ; description
  ; ip_permissions
  }

let parse xml =
  Some
    { group_id =
        Aws.Xml.required
          "groupId"
          (Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse)
    ; ip_permissions_egress =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipPermissionsEgress" xml)
             IpPermissionList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; security_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "securityGroupArn" xml) String.parse
    ; owner_id =
        Aws.Xml.required
          "ownerId"
          (Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse)
    ; group_name =
        Aws.Xml.required
          "groupName"
          (Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse)
    ; description =
        Aws.Xml.required
          "groupDescription"
          (Aws.Util.option_bind (Aws.Xml.member "groupDescription" xml) String.parse)
    ; ip_permissions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipPermissions" xml)
             IpPermissionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("IpPermissions", IpPermissionList.to_query v.ip_permissions))
       ; Some (Aws.Query.Pair ("GroupDescription", String.to_query v.description))
       ; Some (Aws.Query.Pair ("GroupName", String.to_query v.group_name))
       ; Some (Aws.Query.Pair ("OwnerId", String.to_query v.owner_id))
       ; Aws.Util.option_map v.security_group_arn (fun f ->
             Aws.Query.Pair ("SecurityGroupArn", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ("IpPermissionsEgress", IpPermissionList.to_query v.ip_permissions_egress))
       ; Some (Aws.Query.Pair ("GroupId", String.to_query v.group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ipPermissions", IpPermissionList.to_json v.ip_permissions)
       ; Some ("groupDescription", String.to_json v.description)
       ; Some ("groupName", String.to_json v.group_name)
       ; Some ("ownerId", String.to_json v.owner_id)
       ; Aws.Util.option_map v.security_group_arn (fun f ->
             "securityGroupArn", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Some ("ipPermissionsEgress", IpPermissionList.to_json v.ip_permissions_egress)
       ; Some ("groupId", String.to_json v.group_id)
       ])

let of_json j =
  { group_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupId"))
  ; ip_permissions_egress =
      IpPermissionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipPermissionsEgress"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; security_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "securityGroupArn") String.of_json
  ; owner_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ownerId"))
  ; group_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupName"))
  ; description =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupDescription"))
  ; ip_permissions =
      IpPermissionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipPermissions"))
  }
