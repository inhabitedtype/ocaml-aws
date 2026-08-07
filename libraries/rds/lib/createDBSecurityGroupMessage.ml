open Aws.BaseTypes

type t =
  { d_b_security_group_name : String.t
  ; d_b_security_group_description : String.t
  ; tags : TagList.t
  }

let make ~d_b_security_group_name ~d_b_security_group_description ?(tags = []) () =
  { d_b_security_group_name; d_b_security_group_description; tags }

let parse xml =
  Some
    { d_b_security_group_name =
        Aws.Xml.required
          "DBSecurityGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "DBSecurityGroupName" xml) String.parse)
    ; d_b_security_group_description =
        Aws.Xml.required
          "DBSecurityGroupDescription"
          (Aws.Util.option_bind
             (Aws.Xml.member "DBSecurityGroupDescription" xml)
             String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ( "DBSecurityGroupDescription"
              , String.to_query v.d_b_security_group_description ))
       ; Some
           (Aws.Query.Pair
              ("DBSecurityGroupName", String.to_query v.d_b_security_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some
           ("DBSecurityGroupDescription", String.to_json v.d_b_security_group_description)
       ; Some ("DBSecurityGroupName", String.to_json v.d_b_security_group_name)
       ])

let of_json j =
  { d_b_security_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSecurityGroupName"))
  ; d_b_security_group_description =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSecurityGroupDescription"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
