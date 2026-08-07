open Aws.BaseTypes

type t =
  { execution_status_filter : StackRefactorExecutionStatusFilter.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make ?(execution_status_filter = []) ?next_token ?max_results () =
  { execution_status_filter; next_token; max_results }

let parse xml =
  Some
    { execution_status_filter =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ExecutionStatusFilter" xml)
             StackRefactorExecutionStatusFilter.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ExecutionStatusFilter.member"
              , StackRefactorExecutionStatusFilter.to_query v.execution_status_filter ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "ExecutionStatusFilter"
           , StackRefactorExecutionStatusFilter.to_json v.execution_status_filter )
       ])

let of_json j =
  { execution_status_filter =
      StackRefactorExecutionStatusFilter.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ExecutionStatusFilter"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
