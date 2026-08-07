open Aws.BaseTypes

type t = { access_key_id : String.t }

let make ~access_key_id () = { access_key_id }

let parse xml =
  Some
    { access_key_id =
        Aws.Xml.required
          "AccessKeyId"
          (Aws.Util.option_bind (Aws.Xml.member "AccessKeyId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("AccessKeyId", String.to_query v.access_key_id)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("AccessKeyId", String.to_json v.access_key_id) ])

let of_json j =
  { access_key_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AccessKeyId"))
  }
