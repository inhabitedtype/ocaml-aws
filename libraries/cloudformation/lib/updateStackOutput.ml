open Aws.BaseTypes

type t =
  { stack_id : String.t option
  ; operation_id : String.t option
  }

let make ?stack_id ?operation_id () = { stack_id; operation_id }

let parse xml =
  Some
    { stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
    ; operation_id = Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operation_id (fun f ->
             Aws.Query.Pair ("OperationId", String.to_query f))
       ; Aws.Util.option_map v.stack_id (fun f ->
             Aws.Query.Pair ("StackId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
       ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
       ])

let of_json j =
  { stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
  ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
  }
