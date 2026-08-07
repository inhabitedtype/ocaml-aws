open Aws.BaseTypes

type t =
  { resource : String.t
  ; use_long_ids : Boolean.t
  ; principal_arn : String.t
  }

let make ~resource ~use_long_ids ~principal_arn () =
  { resource; use_long_ids; principal_arn }

let parse xml =
  Some
    { resource =
        Aws.Xml.required
          "resource"
          (Aws.Util.option_bind (Aws.Xml.member "resource" xml) String.parse)
    ; use_long_ids =
        Aws.Xml.required
          "useLongIds"
          (Aws.Util.option_bind (Aws.Xml.member "useLongIds" xml) Boolean.parse)
    ; principal_arn =
        Aws.Xml.required
          "principalArn"
          (Aws.Util.option_bind (Aws.Xml.member "principalArn" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PrincipalArn", String.to_query v.principal_arn))
       ; Some (Aws.Query.Pair ("UseLongIds", Boolean.to_query v.use_long_ids))
       ; Some (Aws.Query.Pair ("Resource", String.to_query v.resource))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("principalArn", String.to_json v.principal_arn)
       ; Some ("useLongIds", Boolean.to_json v.use_long_ids)
       ; Some ("resource", String.to_json v.resource)
       ])

let of_json j =
  { resource = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "resource"))
  ; use_long_ids =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "useLongIds"))
  ; principal_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "principalArn"))
  }
