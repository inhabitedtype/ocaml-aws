open Aws.BaseTypes

type t =
  { change_set_name : String.t option
  ; change_set_id : String.t option
  ; stack_id : String.t option
  ; stack_name : String.t option
  ; description : String.t option
  ; parameters : Parameters.t
  ; creation_time : DateTime.t option
  ; execution_status : ExecutionStatus.t option
  ; status : ChangeSetStatus.t option
  ; status_reason : String.t option
  ; stack_drift_status : StackDriftStatus.t option
  ; notification_a_r_ns : NotificationARNs.t
  ; rollback_configuration : RollbackConfiguration.t option
  ; capabilities : Capabilities.t
  ; tags : Tags.t
  ; changes : Changes.t
  ; next_token : String.t option
  ; include_nested_stacks : Boolean.t option
  ; parent_change_set_id : String.t option
  ; root_change_set_id : String.t option
  ; on_stack_failure : OnStackFailure.t option
  ; import_existing_resources : Boolean.t option
  ; deployment_mode : DeploymentMode.t option
  }

let make
    ?change_set_name
    ?change_set_id
    ?stack_id
    ?stack_name
    ?description
    ?(parameters = [])
    ?creation_time
    ?execution_status
    ?status
    ?status_reason
    ?stack_drift_status
    ?(notification_a_r_ns = [])
    ?rollback_configuration
    ?(capabilities = [])
    ?(tags = [])
    ?(changes = [])
    ?next_token
    ?include_nested_stacks
    ?parent_change_set_id
    ?root_change_set_id
    ?on_stack_failure
    ?import_existing_resources
    ?deployment_mode
    () =
  { change_set_name
  ; change_set_id
  ; stack_id
  ; stack_name
  ; description
  ; parameters
  ; creation_time
  ; execution_status
  ; status
  ; status_reason
  ; stack_drift_status
  ; notification_a_r_ns
  ; rollback_configuration
  ; capabilities
  ; tags
  ; changes
  ; next_token
  ; include_nested_stacks
  ; parent_change_set_id
  ; root_change_set_id
  ; on_stack_failure
  ; import_existing_resources
  ; deployment_mode
  }

let parse xml =
  Some
    { change_set_name =
        Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse
    ; change_set_id = Aws.Util.option_bind (Aws.Xml.member "ChangeSetId" xml) String.parse
    ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
    ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse
    ; execution_status =
        Aws.Util.option_bind (Aws.Xml.member "ExecutionStatus" xml) ExecutionStatus.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) ChangeSetStatus.parse
    ; status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
    ; stack_drift_status =
        Aws.Util.option_bind
          (Aws.Xml.member "StackDriftStatus" xml)
          StackDriftStatus.parse
    ; notification_a_r_ns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NotificationARNs" xml)
             NotificationARNs.parse)
    ; rollback_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "RollbackConfiguration" xml)
          RollbackConfiguration.parse
    ; capabilities =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
    ; changes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Changes" xml) Changes.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; include_nested_stacks =
        Aws.Util.option_bind (Aws.Xml.member "IncludeNestedStacks" xml) Boolean.parse
    ; parent_change_set_id =
        Aws.Util.option_bind (Aws.Xml.member "ParentChangeSetId" xml) String.parse
    ; root_change_set_id =
        Aws.Util.option_bind (Aws.Xml.member "RootChangeSetId" xml) String.parse
    ; on_stack_failure =
        Aws.Util.option_bind (Aws.Xml.member "OnStackFailure" xml) OnStackFailure.parse
    ; import_existing_resources =
        Aws.Util.option_bind (Aws.Xml.member "ImportExistingResources" xml) Boolean.parse
    ; deployment_mode =
        Aws.Util.option_bind (Aws.Xml.member "DeploymentMode" xml) DeploymentMode.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.deployment_mode (fun f ->
             Aws.Query.Pair ("DeploymentMode", DeploymentMode.to_query f))
       ; Aws.Util.option_map v.import_existing_resources (fun f ->
             Aws.Query.Pair ("ImportExistingResources", Boolean.to_query f))
       ; Aws.Util.option_map v.on_stack_failure (fun f ->
             Aws.Query.Pair ("OnStackFailure", OnStackFailure.to_query f))
       ; Aws.Util.option_map v.root_change_set_id (fun f ->
             Aws.Query.Pair ("RootChangeSetId", String.to_query f))
       ; Aws.Util.option_map v.parent_change_set_id (fun f ->
             Aws.Query.Pair ("ParentChangeSetId", String.to_query f))
       ; Aws.Util.option_map v.include_nested_stacks (fun f ->
             Aws.Query.Pair ("IncludeNestedStacks", Boolean.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Changes.member", Changes.to_query v.changes))
       ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
       ; Some
           (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
       ; Aws.Util.option_map v.rollback_configuration (fun f ->
             Aws.Query.Pair ("RollbackConfiguration", RollbackConfiguration.to_query f))
       ; Some
           (Aws.Query.Pair
              ("NotificationARNs.member", NotificationARNs.to_query v.notification_a_r_ns))
       ; Aws.Util.option_map v.stack_drift_status (fun f ->
             Aws.Query.Pair ("StackDriftStatus", StackDriftStatus.to_query f))
       ; Aws.Util.option_map v.status_reason (fun f ->
             Aws.Query.Pair ("StatusReason", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ChangeSetStatus.to_query f))
       ; Aws.Util.option_map v.execution_status (fun f ->
             Aws.Query.Pair ("ExecutionStatus", ExecutionStatus.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ; Aws.Util.option_map v.stack_id (fun f ->
             Aws.Query.Pair ("StackId", String.to_query f))
       ; Aws.Util.option_map v.change_set_id (fun f ->
             Aws.Query.Pair ("ChangeSetId", String.to_query f))
       ; Aws.Util.option_map v.change_set_name (fun f ->
             Aws.Query.Pair ("ChangeSetName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.deployment_mode (fun f ->
             "DeploymentMode", DeploymentMode.to_json f)
       ; Aws.Util.option_map v.import_existing_resources (fun f ->
             "ImportExistingResources", Boolean.to_json f)
       ; Aws.Util.option_map v.on_stack_failure (fun f ->
             "OnStackFailure", OnStackFailure.to_json f)
       ; Aws.Util.option_map v.root_change_set_id (fun f ->
             "RootChangeSetId", String.to_json f)
       ; Aws.Util.option_map v.parent_change_set_id (fun f ->
             "ParentChangeSetId", String.to_json f)
       ; Aws.Util.option_map v.include_nested_stacks (fun f ->
             "IncludeNestedStacks", Boolean.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Changes", Changes.to_json v.changes)
       ; Some ("Tags", Tags.to_json v.tags)
       ; Some ("Capabilities", Capabilities.to_json v.capabilities)
       ; Aws.Util.option_map v.rollback_configuration (fun f ->
             "RollbackConfiguration", RollbackConfiguration.to_json f)
       ; Some ("NotificationARNs", NotificationARNs.to_json v.notification_a_r_ns)
       ; Aws.Util.option_map v.stack_drift_status (fun f ->
             "StackDriftStatus", StackDriftStatus.to_json f)
       ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", ChangeSetStatus.to_json f)
       ; Aws.Util.option_map v.execution_status (fun f ->
             "ExecutionStatus", ExecutionStatus.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "CreationTime", DateTime.to_json f)
       ; Some ("Parameters", Parameters.to_json v.parameters)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
       ; Aws.Util.option_map v.change_set_id (fun f -> "ChangeSetId", String.to_json f)
       ; Aws.Util.option_map v.change_set_name (fun f ->
             "ChangeSetName", String.to_json f)
       ])

let of_json j =
  { change_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "ChangeSetName") String.of_json
  ; change_set_id = Aws.Util.option_map (Aws.Json.lookup j "ChangeSetId") String.of_json
  ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
  ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; parameters =
      Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "CreationTime") DateTime.of_json
  ; execution_status =
      Aws.Util.option_map (Aws.Json.lookup j "ExecutionStatus") ExecutionStatus.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") ChangeSetStatus.of_json
  ; status_reason = Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
  ; stack_drift_status =
      Aws.Util.option_map (Aws.Json.lookup j "StackDriftStatus") StackDriftStatus.of_json
  ; notification_a_r_ns =
      NotificationARNs.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationARNs"))
  ; rollback_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "RollbackConfiguration")
        RollbackConfiguration.of_json
  ; capabilities =
      Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
  ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; changes = Changes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Changes"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; include_nested_stacks =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeNestedStacks") Boolean.of_json
  ; parent_change_set_id =
      Aws.Util.option_map (Aws.Json.lookup j "ParentChangeSetId") String.of_json
  ; root_change_set_id =
      Aws.Util.option_map (Aws.Json.lookup j "RootChangeSetId") String.of_json
  ; on_stack_failure =
      Aws.Util.option_map (Aws.Json.lookup j "OnStackFailure") OnStackFailure.of_json
  ; import_existing_resources =
      Aws.Util.option_map (Aws.Json.lookup j "ImportExistingResources") Boolean.of_json
  ; deployment_mode =
      Aws.Util.option_map (Aws.Json.lookup j "DeploymentMode") DeploymentMode.of_json
  }
