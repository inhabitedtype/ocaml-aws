open Aws.BaseTypes

type t =
  { queue_url : String.t
  ; label : String.t
  }

let make ~queue_url ~label () = { queue_url; label }

let parse xml =
  Some
    { queue_url =
        Aws.Xml.required
          "QueueUrl"
          (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
    ; label =
        Aws.Xml.required
          "Label"
          (Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Label", String.to_query v.label))
       ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Label", String.to_json v.label)
       ; Some ("QueueUrl", String.to_json v.queue_url)
       ])

let of_json j =
  { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
  ; label = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Label"))
  }
