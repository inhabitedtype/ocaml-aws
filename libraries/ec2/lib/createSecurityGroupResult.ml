open Aws.BaseTypes

type t =
  { group_id : String.t
  ; tags : TagList.t
  ; security_group_arn : String.t option
  }

let make ~group_id ?(tags = []) ?security_group_arn () =
  { group_id; tags; security_group_arn }

let parse xml =
  Some
    { group_id =
        Aws.Xml.required
          "groupId"
          (Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; security_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "securityGroupArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.security_group_arn (fun f ->
             Aws.Query.Pair ("SecurityGroupArn", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("GroupId", String.to_query v.group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.security_group_arn (fun f ->
             "securityGroupArn", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Some ("groupId", String.to_json v.group_id)
       ])

let of_json j =
  { group_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupId"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; security_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "securityGroupArn") String.of_json
  }
