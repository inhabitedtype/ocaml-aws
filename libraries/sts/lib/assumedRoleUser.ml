open Aws.BaseTypes

type t =
  { assumed_role_id : String.t
  ; arn : String.t
  }

let make ~assumed_role_id ~arn () = { assumed_role_id; arn }

let parse xml =
  Some
    { assumed_role_id =
        Aws.Xml.required
          "AssumedRoleId"
          (Aws.Util.option_bind (Aws.Xml.member "AssumedRoleId" xml) String.parse)
    ; arn =
        Aws.Xml.required
          "Arn"
          (Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Arn", String.to_query v.arn))
       ; Some (Aws.Query.Pair ("AssumedRoleId", String.to_query v.assumed_role_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Arn", String.to_json v.arn)
       ; Some ("AssumedRoleId", String.to_json v.assumed_role_id)
       ])

let of_json j =
  { assumed_role_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AssumedRoleId"))
  ; arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Arn"))
  }
