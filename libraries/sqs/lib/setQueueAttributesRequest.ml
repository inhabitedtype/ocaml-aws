open Aws.BaseTypes

type t =
  { queue_url : String.t
  ; attributes : QueueAttributeMap.t
  }

let make ~queue_url ~attributes () = { queue_url; attributes }

let parse xml =
  Some
    { queue_url =
        Aws.Xml.required
          "QueueUrl"
          (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
    ; attributes =
        Aws.Xml.required
          "Attributes"
          (Aws.Util.option_bind (Aws.Xml.member "Attributes" xml) QueueAttributeMap.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Attributes", QueueAttributeMap.to_query v.attributes))
       ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Attributes", QueueAttributeMap.to_json v.attributes)
       ; Some ("QueueUrl", String.to_json v.queue_url)
       ])

let of_json j =
  { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
  ; attributes =
      QueueAttributeMap.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Attributes"))
  }
