open Aws.BaseTypes

type t =
  { log_role_arn : String.t
  ; log_group_name : String.t
  }

let make ~log_role_arn ~log_group_name () = { log_role_arn; log_group_name }

let parse xml =
  Some
    { log_role_arn =
        Aws.Xml.required
          "LogRoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "LogRoleArn" xml) String.parse)
    ; log_group_name =
        Aws.Xml.required
          "LogGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "LogGroupName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("LogGroupName", String.to_query v.log_group_name))
       ; Some (Aws.Query.Pair ("LogRoleArn", String.to_query v.log_role_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LogGroupName", String.to_json v.log_group_name)
       ; Some ("LogRoleArn", String.to_json v.log_role_arn)
       ])

let of_json j =
  { log_role_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogRoleArn"))
  ; log_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogGroupName"))
  }
