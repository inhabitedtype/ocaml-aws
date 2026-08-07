open Aws.BaseTypes

type t =
  { queue_url : String.t
  ; entries : SendMessageBatchRequestEntryList.t
  }

let make ~queue_url ~entries () = { queue_url; entries }

let parse xml =
  Some
    { queue_url =
        Aws.Xml.required
          "QueueUrl"
          (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
    ; entries = Aws.Xml.required "Entries" (SendMessageBatchRequestEntryList.parse xml)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Entries.member", SendMessageBatchRequestEntryList.to_query v.entries))
       ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Entries", SendMessageBatchRequestEntryList.to_json v.entries)
       ; Some ("QueueUrl", String.to_json v.queue_url)
       ])

let of_json j =
  { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
  ; entries =
      SendMessageBatchRequestEntryList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Entries"))
  }
