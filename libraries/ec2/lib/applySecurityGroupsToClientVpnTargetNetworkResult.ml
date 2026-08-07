type t = { security_group_ids : ClientVpnSecurityGroupIdSet.t }

let make ?(security_group_ids = []) () = { security_group_ids }

let parse xml =
  Some
    { security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupIds" xml)
             ClientVpnSecurityGroupIdSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SecurityGroupIds"
              , ClientVpnSecurityGroupIdSet.to_query v.security_group_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("securityGroupIds", ClientVpnSecurityGroupIdSet.to_json v.security_group_ids)
       ])

let of_json j =
  { security_group_ids =
      ClientVpnSecurityGroupIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupIds"))
  }
