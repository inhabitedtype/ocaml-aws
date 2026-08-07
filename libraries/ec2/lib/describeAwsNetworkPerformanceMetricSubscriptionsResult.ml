open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; subscriptions : SubscriptionList.t
  }

let make ?next_token ?(subscriptions = []) () = { next_token; subscriptions }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; subscriptions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "subscriptionSet" xml)
             SubscriptionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("SubscriptionSet", SubscriptionList.to_query v.subscriptions))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("subscriptionSet", SubscriptionList.to_json v.subscriptions)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; subscriptions =
      SubscriptionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "subscriptionSet"))
  }
