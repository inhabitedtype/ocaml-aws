open Aws.BaseTypes

type t =
  { stack_name : String.t option
  ; change_set_name : String.t option
  ; operation_id : String.t option
  ; filters : EventFilter.t option
  ; next_token : String.t option
  }

let make ?stack_name ?change_set_name ?operation_id ?filters ?next_token () =
  { stack_name; change_set_name; operation_id; filters; next_token }

let parse xml =
  Some
    { stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    ; change_set_name =
        Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse
    ; operation_id = Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
    ; filters = Aws.Util.option_bind (Aws.Xml.member "Filters" xml) EventFilter.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.filters (fun f ->
             Aws.Query.Pair ("Filters", EventFilter.to_query f))
       ; Aws.Util.option_map v.operation_id (fun f ->
             Aws.Query.Pair ("OperationId", String.to_query f))
       ; Aws.Util.option_map v.change_set_name (fun f ->
             Aws.Query.Pair ("ChangeSetName", String.to_query f))
       ; Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.filters (fun f -> "Filters", EventFilter.to_json f)
       ; Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
       ; Aws.Util.option_map v.change_set_name (fun f ->
             "ChangeSetName", String.to_json f)
       ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ])

let of_json j =
  { stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  ; change_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "ChangeSetName") String.of_json
  ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
  ; filters = Aws.Util.option_map (Aws.Json.lookup j "Filters") EventFilter.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
