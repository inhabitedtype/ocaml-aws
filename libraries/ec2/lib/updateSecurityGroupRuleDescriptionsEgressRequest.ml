open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; group_id : String.t option
  ; group_name : String.t option
  ; ip_permissions : IpPermissionList.t
  ; security_group_rule_descriptions : SecurityGroupRuleDescriptionList.t
  }

let make
    ?dry_run
    ?group_id
    ?group_name
    ?(ip_permissions = [])
    ?(security_group_rule_descriptions = [])
    () =
  { dry_run; group_id; group_name; ip_permissions; security_group_rule_descriptions }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "GroupId" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "GroupName" xml) String.parse
    ; ip_permissions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "IpPermissions" xml)
             IpPermissionList.parse)
    ; security_group_rule_descriptions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupRuleDescription" xml)
             SecurityGroupRuleDescriptionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SecurityGroupRuleDescription"
              , SecurityGroupRuleDescriptionList.to_query
                  v.security_group_rule_descriptions ))
       ; Some
           (Aws.Query.Pair ("IpPermissions", IpPermissionList.to_query v.ip_permissions))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "SecurityGroupRuleDescription"
           , SecurityGroupRuleDescriptionList.to_json v.security_group_rule_descriptions
           )
       ; Some ("IpPermissions", IpPermissionList.to_json v.ip_permissions)
       ; Aws.Util.option_map v.group_name (fun f -> "GroupName", String.to_json f)
       ; Aws.Util.option_map v.group_id (fun f -> "GroupId", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "GroupId") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "GroupName") String.of_json
  ; ip_permissions =
      IpPermissionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IpPermissions"))
  ; security_group_rule_descriptions =
      SecurityGroupRuleDescriptionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupRuleDescription"))
  }
