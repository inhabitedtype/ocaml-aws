open Aws.BaseTypes

type t =
  { queue_url : String.t
  ; receipt_handle : String.t
  }

let make ~queue_url ~receipt_handle () = { queue_url; receipt_handle }

let parse xml =
  Some
    { queue_url =
        Aws.Xml.required
          "QueueUrl"
          (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
    ; receipt_handle =
        Aws.Xml.required
          "ReceiptHandle"
          (Aws.Util.option_bind (Aws.Xml.member "ReceiptHandle" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ReceiptHandle", String.to_query v.receipt_handle))
       ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ReceiptHandle", String.to_json v.receipt_handle)
       ; Some ("QueueUrl", String.to_json v.queue_url)
       ])

let of_json j =
  { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
  ; receipt_handle =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReceiptHandle"))
  }
