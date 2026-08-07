open Aws.BaseTypes

type t =
  { stack_set_name : String.t
  ; operation_preferences : StackSetOperationPreferences.t option
  ; operation_id : String.t option
  ; call_as : CallAs.t option
  }

let make ~stack_set_name ?operation_preferences ?operation_id ?call_as () =
  { stack_set_name; operation_preferences; operation_id; call_as }

let parse xml =
  Some
    { stack_set_name =
        Aws.Xml.required
          "StackSetName"
          (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
    ; operation_preferences =
        Aws.Util.option_bind
          (Aws.Xml.member "OperationPreferences" xml)
          StackSetOperationPreferences.parse
    ; operation_id = Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
    ; call_as = Aws.Util.option_bind (Aws.Xml.member "CallAs" xml) CallAs.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f ->
             Aws.Query.Pair ("CallAs", CallAs.to_query f))
       ; Aws.Util.option_map v.operation_id (fun f ->
             Aws.Query.Pair ("OperationId", String.to_query f))
       ; Aws.Util.option_map v.operation_preferences (fun f ->
             Aws.Query.Pair
               ("OperationPreferences", StackSetOperationPreferences.to_query f))
       ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f -> "CallAs", CallAs.to_json f)
       ; Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
       ; Aws.Util.option_map v.operation_preferences (fun f ->
             "OperationPreferences", StackSetOperationPreferences.to_json f)
       ; Some ("StackSetName", String.to_json v.stack_set_name)
       ])

let of_json j =
  { stack_set_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
  ; operation_preferences =
      Aws.Util.option_map
        (Aws.Json.lookup j "OperationPreferences")
        StackSetOperationPreferences.of_json
  ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
  ; call_as = Aws.Util.option_map (Aws.Json.lookup j "CallAs") CallAs.of_json
  }
