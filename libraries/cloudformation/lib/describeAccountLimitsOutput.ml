open Aws.BaseTypes

type t =
  { account_limits : AccountLimitList.t
  ; next_token : String.t option
  }

let make ?(account_limits = []) ?next_token () = { account_limits; next_token }

let parse xml =
  Some
    { account_limits =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AccountLimits" xml)
             AccountLimitList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AccountLimits.member", AccountLimitList.to_query v.account_limits))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("AccountLimits", AccountLimitList.to_json v.account_limits)
       ])

let of_json j =
  { account_limits =
      AccountLimitList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AccountLimits"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
