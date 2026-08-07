open Aws.BaseTypes

type t =
  { alias_name : String.t
  ; target_key_id : String.t
  }

let make ~alias_name ~target_key_id () = { alias_name; target_key_id }

let parse xml =
  Some
    { alias_name =
        Aws.Xml.required
          "AliasName"
          (Aws.Util.option_bind (Aws.Xml.member "AliasName" xml) String.parse)
    ; target_key_id =
        Aws.Xml.required
          "TargetKeyId"
          (Aws.Util.option_bind (Aws.Xml.member "TargetKeyId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TargetKeyId", String.to_query v.target_key_id))
       ; Some (Aws.Query.Pair ("AliasName", String.to_query v.alias_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TargetKeyId", String.to_json v.target_key_id)
       ; Some ("AliasName", String.to_json v.alias_name)
       ])

let of_json j =
  { alias_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AliasName"))
  ; target_key_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetKeyId"))
  }
