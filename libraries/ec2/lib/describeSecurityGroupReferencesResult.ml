type t = { security_group_reference_set : SecurityGroupReferences.t }

let make ?(security_group_reference_set = []) () = { security_group_reference_set }

let parse xml =
  Some
    { security_group_reference_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupReferenceSet" xml)
             SecurityGroupReferences.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SecurityGroupReferenceSet"
              , SecurityGroupReferences.to_query v.security_group_reference_set ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "securityGroupReferenceSet"
           , SecurityGroupReferences.to_json v.security_group_reference_set )
       ])

let of_json j =
  { security_group_reference_set =
      SecurityGroupReferences.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupReferenceSet"))
  }
