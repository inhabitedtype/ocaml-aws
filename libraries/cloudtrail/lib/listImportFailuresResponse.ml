open Aws.BaseTypes

type t =
  { failures : ImportFailureList.t
  ; next_token : String.t option
  }

let make ?(failures = []) ?next_token () = { failures; next_token }

let parse xml =
  Some
    { failures =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Failures" xml) ImportFailureList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Failures.member", ImportFailureList.to_query v.failures))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Failures", ImportFailureList.to_json v.failures)
       ])

let of_json j =
  { failures =
      ImportFailureList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Failures"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
