open Aws.BaseTypes

type t = { user_id : String.t }

let make ~user_id () = { user_id }

let parse xml =
  Some
    { user_id =
        Aws.Xml.required
          "UserId"
          (Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("UserId", String.to_query v.user_id)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("UserId", String.to_json v.user_id) ])

let of_json j =
  { user_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserId")) }
