open Aws.BaseTypes

type t =
  { queue_url : String.t
  ; tags : TagMap.t
  }

let make ~queue_url ~tags () = { queue_url; tags }

let parse xml =
  Some
    { queue_url =
        Aws.Xml.required
          "QueueUrl"
          (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
    ; tags =
        Aws.Xml.required
          "Tags"
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagMap.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags", TagMap.to_query v.tags))
       ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagMap.to_json v.tags)
       ; Some ("QueueUrl", String.to_json v.queue_url)
       ])

let of_json j =
  { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
  ; tags = TagMap.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
