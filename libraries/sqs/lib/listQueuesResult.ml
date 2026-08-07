open Aws.BaseTypes

type t =
  { queue_urls : QueueUrlList.t
  ; next_token : String.t option
  }

let make ?(queue_urls = []) ?next_token () = { queue_urls; next_token }

let parse xml =
  Some
    { queue_urls = Aws.Util.of_option [] (QueueUrlList.parse xml)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("QueueUrls.member", QueueUrlList.to_query v.queue_urls))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("QueueUrls", QueueUrlList.to_json v.queue_urls)
       ])

let of_json j =
  { queue_urls =
      QueueUrlList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrls"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
