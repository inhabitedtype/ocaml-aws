open Aws.BaseTypes

type t = { delegated_admin_account_id : String.t }

let make ~delegated_admin_account_id () = { delegated_admin_account_id }

let parse xml =
  Some
    { delegated_admin_account_id =
        Aws.Xml.required
          "DelegatedAdminAccountId"
          (Aws.Util.option_bind
             (Aws.Xml.member "DelegatedAdminAccountId" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("DelegatedAdminAccountId", String.to_query v.delegated_admin_account_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DelegatedAdminAccountId", String.to_json v.delegated_admin_account_id) ])

let of_json j =
  { delegated_admin_account_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DelegatedAdminAccountId"))
  }
