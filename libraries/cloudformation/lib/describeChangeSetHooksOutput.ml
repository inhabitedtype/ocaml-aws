open Aws.BaseTypes

type t =
  { change_set_id : String.t option
  ; change_set_name : String.t option
  ; hooks : ChangeSetHooks.t
  ; status : ChangeSetHooksStatus.t option
  ; next_token : String.t option
  ; stack_id : String.t option
  ; stack_name : String.t option
  }

let make
    ?change_set_id
    ?change_set_name
    ?(hooks = [])
    ?status
    ?next_token
    ?stack_id
    ?stack_name
    () =
  { change_set_id; change_set_name; hooks; status; next_token; stack_id; stack_name }

let parse xml =
  Some
    { change_set_id = Aws.Util.option_bind (Aws.Xml.member "ChangeSetId" xml) String.parse
    ; change_set_name =
        Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse
    ; hooks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Hooks" xml) ChangeSetHooks.parse)
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) ChangeSetHooksStatus.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
    ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ; Aws.Util.option_map v.stack_id (fun f ->
             Aws.Query.Pair ("StackId", String.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ChangeSetHooksStatus.to_query f))
       ; Some (Aws.Query.Pair ("Hooks.member", ChangeSetHooks.to_query v.hooks))
       ; Aws.Util.option_map v.change_set_name (fun f ->
             Aws.Query.Pair ("ChangeSetName", String.to_query f))
       ; Aws.Util.option_map v.change_set_id (fun f ->
             Aws.Query.Pair ("ChangeSetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", ChangeSetHooksStatus.to_json f)
       ; Some ("Hooks", ChangeSetHooks.to_json v.hooks)
       ; Aws.Util.option_map v.change_set_name (fun f ->
             "ChangeSetName", String.to_json f)
       ; Aws.Util.option_map v.change_set_id (fun f -> "ChangeSetId", String.to_json f)
       ])

let of_json j =
  { change_set_id = Aws.Util.option_map (Aws.Json.lookup j "ChangeSetId") String.of_json
  ; change_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "ChangeSetName") String.of_json
  ; hooks = ChangeSetHooks.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Hooks"))
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") ChangeSetHooksStatus.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
  ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  }
