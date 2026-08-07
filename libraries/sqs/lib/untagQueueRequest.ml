open Aws.BaseTypes

type t =
  { queue_url : String.t
  ; tag_keys : TagKeyList.t
  }

let make ~queue_url ~tag_keys () = { queue_url; tag_keys }

let parse xml =
  Some
    { queue_url =
        Aws.Xml.required
          "QueueUrl"
          (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
    ; tag_keys = Aws.Xml.required "TagKeys" (TagKeyList.parse xml)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagKeys.member", TagKeyList.to_query v.tag_keys))
       ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagKeys", TagKeyList.to_json v.tag_keys)
       ; Some ("QueueUrl", String.to_json v.queue_url)
       ])

let of_json j =
  { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
  ; tag_keys = TagKeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagKeys"))
  }
