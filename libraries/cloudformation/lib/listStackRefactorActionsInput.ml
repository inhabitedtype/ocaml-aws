open Aws.BaseTypes

type t =
  { stack_refactor_id : String.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make ~stack_refactor_id ?next_token ?max_results () =
  { stack_refactor_id; next_token; max_results }

let parse xml =
  Some
    { stack_refactor_id =
        Aws.Xml.required
          "StackRefactorId"
          (Aws.Util.option_bind (Aws.Xml.member "StackRefactorId" xml) String.parse)
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
       ; Some (Aws.Query.Pair ("StackRefactorId", String.to_query v.stack_refactor_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("StackRefactorId", String.to_json v.stack_refactor_id)
       ])

let of_json j =
  { stack_refactor_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackRefactorId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
