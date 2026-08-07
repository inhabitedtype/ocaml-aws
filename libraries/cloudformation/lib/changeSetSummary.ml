open Aws.BaseTypes

type t =
  { stack_id : String.t option
  ; stack_name : String.t option
  ; change_set_id : String.t option
  ; change_set_name : String.t option
  ; execution_status : ExecutionStatus.t option
  ; status : ChangeSetStatus.t option
  ; status_reason : String.t option
  ; creation_time : DateTime.t option
  ; description : String.t option
  ; include_nested_stacks : Boolean.t option
  ; parent_change_set_id : String.t option
  ; root_change_set_id : String.t option
  ; import_existing_resources : Boolean.t option
  }

let make
    ?stack_id
    ?stack_name
    ?change_set_id
    ?change_set_name
    ?execution_status
    ?status
    ?status_reason
    ?creation_time
    ?description
    ?include_nested_stacks
    ?parent_change_set_id
    ?root_change_set_id
    ?import_existing_resources
    () =
  { stack_id
  ; stack_name
  ; change_set_id
  ; change_set_name
  ; execution_status
  ; status
  ; status_reason
  ; creation_time
  ; description
  ; include_nested_stacks
  ; parent_change_set_id
  ; root_change_set_id
  ; import_existing_resources
  }

let parse xml =
  Some
    { stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
    ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    ; change_set_id = Aws.Util.option_bind (Aws.Xml.member "ChangeSetId" xml) String.parse
    ; change_set_name =
        Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse
    ; execution_status =
        Aws.Util.option_bind (Aws.Xml.member "ExecutionStatus" xml) ExecutionStatus.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) ChangeSetStatus.parse
    ; status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; include_nested_stacks =
        Aws.Util.option_bind (Aws.Xml.member "IncludeNestedStacks" xml) Boolean.parse
    ; parent_change_set_id =
        Aws.Util.option_bind (Aws.Xml.member "ParentChangeSetId" xml) String.parse
    ; root_change_set_id =
        Aws.Util.option_bind (Aws.Xml.member "RootChangeSetId" xml) String.parse
    ; import_existing_resources =
        Aws.Util.option_bind (Aws.Xml.member "ImportExistingResources" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.import_existing_resources (fun f ->
             Aws.Query.Pair ("ImportExistingResources", Boolean.to_query f))
       ; Aws.Util.option_map v.root_change_set_id (fun f ->
             Aws.Query.Pair ("RootChangeSetId", String.to_query f))
       ; Aws.Util.option_map v.parent_change_set_id (fun f ->
             Aws.Query.Pair ("ParentChangeSetId", String.to_query f))
       ; Aws.Util.option_map v.include_nested_stacks (fun f ->
             Aws.Query.Pair ("IncludeNestedStacks", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.status_reason (fun f ->
             Aws.Query.Pair ("StatusReason", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ChangeSetStatus.to_query f))
       ; Aws.Util.option_map v.execution_status (fun f ->
             Aws.Query.Pair ("ExecutionStatus", ExecutionStatus.to_query f))
       ; Aws.Util.option_map v.change_set_name (fun f ->
             Aws.Query.Pair ("ChangeSetName", String.to_query f))
       ; Aws.Util.option_map v.change_set_id (fun f ->
             Aws.Query.Pair ("ChangeSetId", String.to_query f))
       ; Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ; Aws.Util.option_map v.stack_id (fun f ->
             Aws.Query.Pair ("StackId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.import_existing_resources (fun f ->
             "ImportExistingResources", Boolean.to_json f)
       ; Aws.Util.option_map v.root_change_set_id (fun f ->
             "RootChangeSetId", String.to_json f)
       ; Aws.Util.option_map v.parent_change_set_id (fun f ->
             "ParentChangeSetId", String.to_json f)
       ; Aws.Util.option_map v.include_nested_stacks (fun f ->
             "IncludeNestedStacks", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "CreationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", ChangeSetStatus.to_json f)
       ; Aws.Util.option_map v.execution_status (fun f ->
             "ExecutionStatus", ExecutionStatus.to_json f)
       ; Aws.Util.option_map v.change_set_name (fun f ->
             "ChangeSetName", String.to_json f)
       ; Aws.Util.option_map v.change_set_id (fun f -> "ChangeSetId", String.to_json f)
       ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
       ])

let of_json j =
  { stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
  ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  ; change_set_id = Aws.Util.option_map (Aws.Json.lookup j "ChangeSetId") String.of_json
  ; change_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "ChangeSetName") String.of_json
  ; execution_status =
      Aws.Util.option_map (Aws.Json.lookup j "ExecutionStatus") ExecutionStatus.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") ChangeSetStatus.of_json
  ; status_reason = Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "CreationTime") DateTime.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; include_nested_stacks =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeNestedStacks") Boolean.of_json
  ; parent_change_set_id =
      Aws.Util.option_map (Aws.Json.lookup j "ParentChangeSetId") String.of_json
  ; root_change_set_id =
      Aws.Util.option_map (Aws.Json.lookup j "RootChangeSetId") String.of_json
  ; import_existing_resources =
      Aws.Util.option_map (Aws.Json.lookup j "ImportExistingResources") Boolean.of_json
  }
