open Aws.BaseTypes

type t =
  { federated_user_id : String.t
  ; arn : String.t
  }

let make ~federated_user_id ~arn () = { federated_user_id; arn }

let parse xml =
  Some
    { federated_user_id =
        Aws.Xml.required
          "FederatedUserId"
          (Aws.Util.option_bind (Aws.Xml.member "FederatedUserId" xml) String.parse)
    ; arn =
        Aws.Xml.required
          "Arn"
          (Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Arn", String.to_query v.arn))
       ; Some (Aws.Query.Pair ("FederatedUserId", String.to_query v.federated_user_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Arn", String.to_json v.arn)
       ; Some ("FederatedUserId", String.to_json v.federated_user_id)
       ])

let of_json j =
  { federated_user_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FederatedUserId"))
  ; arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Arn"))
  }
