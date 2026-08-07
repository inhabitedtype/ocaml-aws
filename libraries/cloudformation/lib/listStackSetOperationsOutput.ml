open Aws.BaseTypes

type t =
  { summaries : StackSetOperationSummaries.t
  ; next_token : String.t option
  }

let make ?(summaries = []) ?next_token () = { summaries; next_token }

let parse xml =
  Some
    { summaries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Summaries" xml)
             StackSetOperationSummaries.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Summaries.member", StackSetOperationSummaries.to_query v.summaries))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Summaries", StackSetOperationSummaries.to_json v.summaries)
       ])

let of_json j =
  { summaries =
      StackSetOperationSummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Summaries"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
