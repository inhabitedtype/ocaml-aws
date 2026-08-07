open Aws.BaseTypes

type t =
  { stack_resource_summaries : StackResourceSummaries.t
  ; next_token : String.t option
  }

let make ?(stack_resource_summaries = []) ?next_token () =
  { stack_resource_summaries; next_token }

let parse xml =
  Some
    { stack_resource_summaries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "StackResourceSummaries" xml)
             StackResourceSummaries.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "StackResourceSummaries.member"
              , StackResourceSummaries.to_query v.stack_resource_summaries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "StackResourceSummaries"
           , StackResourceSummaries.to_json v.stack_resource_summaries )
       ])

let of_json j =
  { stack_resource_summaries =
      StackResourceSummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceSummaries"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
