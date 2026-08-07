open Aws.BaseTypes

type t =
  { stack_refactor_id : String.t option
  ; description : String.t option
  ; execution_status : StackRefactorExecutionStatus.t option
  ; execution_status_reason : String.t option
  ; status : StackRefactorStatus.t option
  ; status_reason : String.t option
  }

let make
    ?stack_refactor_id
    ?description
    ?execution_status
    ?execution_status_reason
    ?status
    ?status_reason
    () =
  { stack_refactor_id
  ; description
  ; execution_status
  ; execution_status_reason
  ; status
  ; status_reason
  }

let parse xml =
  Some
    { stack_refactor_id =
        Aws.Util.option_bind (Aws.Xml.member "StackRefactorId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; execution_status =
        Aws.Util.option_bind
          (Aws.Xml.member "ExecutionStatus" xml)
          StackRefactorExecutionStatus.parse
    ; execution_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "ExecutionStatusReason" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackRefactorStatus.parse
    ; status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status_reason (fun f ->
             Aws.Query.Pair ("StatusReason", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", StackRefactorStatus.to_query f))
       ; Aws.Util.option_map v.execution_status_reason (fun f ->
             Aws.Query.Pair ("ExecutionStatusReason", String.to_query f))
       ; Aws.Util.option_map v.execution_status (fun f ->
             Aws.Query.Pair ("ExecutionStatus", StackRefactorExecutionStatus.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.stack_refactor_id (fun f ->
             Aws.Query.Pair ("StackRefactorId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", StackRefactorStatus.to_json f)
       ; Aws.Util.option_map v.execution_status_reason (fun f ->
             "ExecutionStatusReason", String.to_json f)
       ; Aws.Util.option_map v.execution_status (fun f ->
             "ExecutionStatus", StackRefactorExecutionStatus.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.stack_refactor_id (fun f ->
             "StackRefactorId", String.to_json f)
       ])

let of_json j =
  { stack_refactor_id =
      Aws.Util.option_map (Aws.Json.lookup j "StackRefactorId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; execution_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "ExecutionStatus")
        StackRefactorExecutionStatus.of_json
  ; execution_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "ExecutionStatusReason") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") StackRefactorStatus.of_json
  ; status_reason = Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
  }
