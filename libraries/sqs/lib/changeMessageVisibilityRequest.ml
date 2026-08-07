open Aws.BaseTypes

type t =
  { queue_url : String.t
  ; receipt_handle : String.t
  ; visibility_timeout : Integer.t
  }

let make ~queue_url ~receipt_handle ~visibility_timeout () =
  { queue_url; receipt_handle; visibility_timeout }

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
    ; visibility_timeout =
        Aws.Xml.required
          "VisibilityTimeout"
          (Aws.Util.option_bind (Aws.Xml.member "VisibilityTimeout" xml) Integer.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("VisibilityTimeout", Integer.to_query v.visibility_timeout))
       ; Some (Aws.Query.Pair ("ReceiptHandle", String.to_query v.receipt_handle))
       ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("VisibilityTimeout", Integer.to_json v.visibility_timeout)
       ; Some ("ReceiptHandle", String.to_json v.receipt_handle)
       ; Some ("QueueUrl", String.to_json v.queue_url)
       ])

let of_json j =
  { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
  ; receipt_handle =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReceiptHandle"))
  ; visibility_timeout =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VisibilityTimeout"))
  }
