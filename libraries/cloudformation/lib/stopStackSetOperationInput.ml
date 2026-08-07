open Aws.BaseTypes

type t =
  { stack_set_name : String.t
  ; operation_id : String.t
  ; call_as : CallAs.t option
  }

let make ~stack_set_name ~operation_id ?call_as () =
  { stack_set_name; operation_id; call_as }

let parse xml =
  Some
    { stack_set_name =
        Aws.Xml.required
          "StackSetName"
          (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
    ; operation_id =
        Aws.Xml.required
          "OperationId"
          (Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse)
    ; call_as = Aws.Util.option_bind (Aws.Xml.member "CallAs" xml) CallAs.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f ->
             Aws.Query.Pair ("CallAs", CallAs.to_query f))
       ; Some (Aws.Query.Pair ("OperationId", String.to_query v.operation_id))
       ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f -> "CallAs", CallAs.to_json f)
       ; Some ("OperationId", String.to_json v.operation_id)
       ; Some ("StackSetName", String.to_json v.stack_set_name)
       ])

let of_json j =
  { stack_set_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
  ; operation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OperationId"))
  ; call_as = Aws.Util.option_map (Aws.Json.lookup j "CallAs") CallAs.of_json
  }
