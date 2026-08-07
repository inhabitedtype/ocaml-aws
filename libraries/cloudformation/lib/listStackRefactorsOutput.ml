open Aws.BaseTypes

type t =
  { stack_refactor_summaries : StackRefactorSummaries.t
  ; next_token : String.t option
  }

let make ~stack_refactor_summaries ?next_token () =
  { stack_refactor_summaries; next_token }

let parse xml =
  Some
    { stack_refactor_summaries =
        Aws.Xml.required
          "StackRefactorSummaries"
          (Aws.Util.option_bind
             (Aws.Xml.member "StackRefactorSummaries" xml)
             StackRefactorSummaries.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "StackRefactorSummaries.member"
              , StackRefactorSummaries.to_query v.stack_refactor_summaries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "StackRefactorSummaries"
           , StackRefactorSummaries.to_json v.stack_refactor_summaries )
       ])

let of_json j =
  { stack_refactor_summaries =
      StackRefactorSummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackRefactorSummaries"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
