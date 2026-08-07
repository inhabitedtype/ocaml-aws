open Aws.BaseTypes

type t =
  { key_id : String.t
  ; tags : TagList.t
  }

let make ~key_id ~tags () = { key_id; tags }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; tags =
        Aws.Xml.required
          "Tags"
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags); Some ("KeyId", String.to_json v.key_id) ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
