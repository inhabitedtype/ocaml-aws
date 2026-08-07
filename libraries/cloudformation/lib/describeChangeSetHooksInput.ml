open Aws.BaseTypes

type t =
  { change_set_name : String.t
  ; stack_name : String.t option
  ; next_token : String.t option
  ; logical_resource_id : String.t option
  }

let make ~change_set_name ?stack_name ?next_token ?logical_resource_id () =
  { change_set_name; stack_name; next_token; logical_resource_id }

let parse xml =
  Some
    { change_set_name =
        Aws.Xml.required
          "ChangeSetName"
          (Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse)
    ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; logical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.logical_resource_id (fun f ->
             Aws.Query.Pair ("LogicalResourceId", String.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ; Some (Aws.Query.Pair ("ChangeSetName", String.to_query v.change_set_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.logical_resource_id (fun f ->
             "LogicalResourceId", String.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ; Some ("ChangeSetName", String.to_json v.change_set_name)
       ])

let of_json j =
  { change_set_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeSetName"))
  ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; logical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "LogicalResourceId") String.of_json
  }
