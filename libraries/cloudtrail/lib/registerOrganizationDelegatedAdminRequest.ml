open Aws.BaseTypes

type t = { member_account_id : String.t }

let make ~member_account_id () = { member_account_id }

let parse xml =
  Some
    { member_account_id =
        Aws.Xml.required
          "MemberAccountId"
          (Aws.Util.option_bind (Aws.Xml.member "MemberAccountId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MemberAccountId", String.to_query v.member_account_id)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MemberAccountId", String.to_json v.member_account_id) ])

let of_json j =
  { member_account_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MemberAccountId"))
  }
