open Aws.BaseTypes

type t =
  { resource_arn : String.t
  ; resource_policy : String.t
  }

let make ~resource_arn ~resource_policy () = { resource_arn; resource_policy }

let parse xml =
  Some
    { resource_arn =
        Aws.Xml.required
          "ResourceArn"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceArn" xml) String.parse)
    ; resource_policy =
        Aws.Xml.required
          "ResourcePolicy"
          (Aws.Util.option_bind (Aws.Xml.member "ResourcePolicy" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ResourcePolicy", String.to_query v.resource_policy))
       ; Some (Aws.Query.Pair ("ResourceArn", String.to_query v.resource_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ResourcePolicy", String.to_json v.resource_policy)
       ; Some ("ResourceArn", String.to_json v.resource_arn)
       ])

let of_json j =
  { resource_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceArn"))
  ; resource_policy =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourcePolicy"))
  }
