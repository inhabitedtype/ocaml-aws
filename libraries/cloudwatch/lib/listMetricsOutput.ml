open Aws.BaseTypes

type t =
  { metrics : Metrics.t
  ; next_token : String.t option
  ; owning_accounts : OwningAccounts.t
  }

let make ?(metrics = []) ?next_token ?(owning_accounts = []) () =
  { metrics; next_token; owning_accounts }

let parse xml =
  Some
    { metrics =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Metrics" xml) Metrics.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; owning_accounts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OwningAccounts" xml)
             OwningAccounts.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("OwningAccounts.member", OwningAccounts.to_query v.owning_accounts))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Metrics.member", Metrics.to_query v.metrics))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("OwningAccounts", OwningAccounts.to_json v.owning_accounts)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Metrics", Metrics.to_json v.metrics)
       ])

let of_json j =
  { metrics = Metrics.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; owning_accounts =
      OwningAccounts.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OwningAccounts"))
  }
