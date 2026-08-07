open Aws.BaseTypes

type t =
  { operation_type : OperationType.t option
  ; operation_id : String.t option
  }

let make ?operation_type ?operation_id () = { operation_type; operation_id }

let parse xml =
  Some
    { operation_type =
        Aws.Util.option_bind (Aws.Xml.member "OperationType" xml) OperationType.parse
    ; operation_id = Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operation_id (fun f ->
             Aws.Query.Pair ("OperationId", String.to_query f))
       ; Aws.Util.option_map v.operation_type (fun f ->
             Aws.Query.Pair ("OperationType", OperationType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
       ; Aws.Util.option_map v.operation_type (fun f ->
             "OperationType", OperationType.to_json f)
       ])

let of_json j =
  { operation_type =
      Aws.Util.option_map (Aws.Json.lookup j "OperationType") OperationType.of_json
  ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
  }
