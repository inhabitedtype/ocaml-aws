open Aws.BaseTypes

type t =
  { return : Boolean.t option
  ; unknown_ip_permissions : IpPermissionList.t
  ; revoked_security_group_rules : RevokedSecurityGroupRuleList.t
  }

let make ?return ?(unknown_ip_permissions = []) ?(revoked_security_group_rules = []) () =
  { return; unknown_ip_permissions; revoked_security_group_rules }

let parse xml =
  Some
    { return = Aws.Util.option_bind (Aws.Xml.member "return" xml) Boolean.parse
    ; unknown_ip_permissions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unknownIpPermissionSet" xml)
             IpPermissionList.parse)
    ; revoked_security_group_rules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "revokedSecurityGroupRuleSet" xml)
             RevokedSecurityGroupRuleList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "RevokedSecurityGroupRuleSet"
              , RevokedSecurityGroupRuleList.to_query v.revoked_security_group_rules ))
       ; Some
           (Aws.Query.Pair
              ( "UnknownIpPermissionSet"
              , IpPermissionList.to_query v.unknown_ip_permissions ))
       ; Aws.Util.option_map v.return (fun f ->
             Aws.Query.Pair ("Return", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "revokedSecurityGroupRuleSet"
           , RevokedSecurityGroupRuleList.to_json v.revoked_security_group_rules )
       ; Some ("unknownIpPermissionSet", IpPermissionList.to_json v.unknown_ip_permissions)
       ; Aws.Util.option_map v.return (fun f -> "return", Boolean.to_json f)
       ])

let of_json j =
  { return = Aws.Util.option_map (Aws.Json.lookup j "return") Boolean.of_json
  ; unknown_ip_permissions =
      IpPermissionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "unknownIpPermissionSet"))
  ; revoked_security_group_rules =
      RevokedSecurityGroupRuleList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "revokedSecurityGroupRuleSet"))
  }
