open Aws.BaseTypes

type t =
  { stack_summaries : StackSummaries.t
  ; next_token : String.t option
  }

let make ?(stack_summaries = []) ?next_token () = { stack_summaries; next_token }

let parse xml =
  Some
    { stack_summaries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "StackSummaries" xml)
             StackSummaries.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("StackSummaries.member", StackSummaries.to_query v.stack_summaries))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("StackSummaries", StackSummaries.to_json v.stack_summaries)
       ])

let of_json j =
  { stack_summaries =
      StackSummaries.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSummaries"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
