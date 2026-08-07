open Aws.BaseTypes

type t =
  { policy_action : PolicyAction.t option
  ; action : ChangeAction.t option
  ; logical_resource_id : String.t option
  ; physical_resource_id : String.t option
  ; resource_type : String.t option
  ; replacement : Replacement.t option
  ; scope : Scope.t
  ; resource_drift_status : StackResourceDriftStatus.t option
  ; resource_drift_ignored_attributes : ResourceDriftIgnoredAttributes.t
  ; details : ResourceChangeDetails.t
  ; change_set_id : String.t option
  ; module_info : ModuleInfo.t option
  ; before_context : String.t option
  ; after_context : String.t option
  ; previous_deployment_context : String.t option
  }

let make
    ?policy_action
    ?action
    ?logical_resource_id
    ?physical_resource_id
    ?resource_type
    ?replacement
    ?(scope = [])
    ?resource_drift_status
    ?(resource_drift_ignored_attributes = [])
    ?(details = [])
    ?change_set_id
    ?module_info
    ?before_context
    ?after_context
    ?previous_deployment_context
    () =
  { policy_action
  ; action
  ; logical_resource_id
  ; physical_resource_id
  ; resource_type
  ; replacement
  ; scope
  ; resource_drift_status
  ; resource_drift_ignored_attributes
  ; details
  ; change_set_id
  ; module_info
  ; before_context
  ; after_context
  ; previous_deployment_context
  }

let parse xml =
  Some
    { policy_action =
        Aws.Util.option_bind (Aws.Xml.member "PolicyAction" xml) PolicyAction.parse
    ; action = Aws.Util.option_bind (Aws.Xml.member "Action" xml) ChangeAction.parse
    ; logical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse
    ; physical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
    ; replacement =
        Aws.Util.option_bind (Aws.Xml.member "Replacement" xml) Replacement.parse
    ; scope =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Scope" xml) Scope.parse)
    ; resource_drift_status =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourceDriftStatus" xml)
          StackResourceDriftStatus.parse
    ; resource_drift_ignored_attributes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceDriftIgnoredAttributes" xml)
             ResourceDriftIgnoredAttributes.parse)
    ; details =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Details" xml)
             ResourceChangeDetails.parse)
    ; change_set_id = Aws.Util.option_bind (Aws.Xml.member "ChangeSetId" xml) String.parse
    ; module_info =
        Aws.Util.option_bind (Aws.Xml.member "ModuleInfo" xml) ModuleInfo.parse
    ; before_context =
        Aws.Util.option_bind (Aws.Xml.member "BeforeContext" xml) String.parse
    ; after_context =
        Aws.Util.option_bind (Aws.Xml.member "AfterContext" xml) String.parse
    ; previous_deployment_context =
        Aws.Util.option_bind (Aws.Xml.member "PreviousDeploymentContext" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.previous_deployment_context (fun f ->
             Aws.Query.Pair ("PreviousDeploymentContext", String.to_query f))
       ; Aws.Util.option_map v.after_context (fun f ->
             Aws.Query.Pair ("AfterContext", String.to_query f))
       ; Aws.Util.option_map v.before_context (fun f ->
             Aws.Query.Pair ("BeforeContext", String.to_query f))
       ; Aws.Util.option_map v.module_info (fun f ->
             Aws.Query.Pair ("ModuleInfo", ModuleInfo.to_query f))
       ; Aws.Util.option_map v.change_set_id (fun f ->
             Aws.Query.Pair ("ChangeSetId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Details.member", ResourceChangeDetails.to_query v.details))
       ; Some
           (Aws.Query.Pair
              ( "ResourceDriftIgnoredAttributes.member"
              , ResourceDriftIgnoredAttributes.to_query
                  v.resource_drift_ignored_attributes ))
       ; Aws.Util.option_map v.resource_drift_status (fun f ->
             Aws.Query.Pair ("ResourceDriftStatus", StackResourceDriftStatus.to_query f))
       ; Some (Aws.Query.Pair ("Scope.member", Scope.to_query v.scope))
       ; Aws.Util.option_map v.replacement (fun f ->
             Aws.Query.Pair ("Replacement", Replacement.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             Aws.Query.Pair ("LogicalResourceId", String.to_query f))
       ; Aws.Util.option_map v.action (fun f ->
             Aws.Query.Pair ("Action", ChangeAction.to_query f))
       ; Aws.Util.option_map v.policy_action (fun f ->
             Aws.Query.Pair ("PolicyAction", PolicyAction.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.previous_deployment_context (fun f ->
             "PreviousDeploymentContext", String.to_json f)
       ; Aws.Util.option_map v.after_context (fun f -> "AfterContext", String.to_json f)
       ; Aws.Util.option_map v.before_context (fun f -> "BeforeContext", String.to_json f)
       ; Aws.Util.option_map v.module_info (fun f -> "ModuleInfo", ModuleInfo.to_json f)
       ; Aws.Util.option_map v.change_set_id (fun f -> "ChangeSetId", String.to_json f)
       ; Some ("Details", ResourceChangeDetails.to_json v.details)
       ; Some
           ( "ResourceDriftIgnoredAttributes"
           , ResourceDriftIgnoredAttributes.to_json v.resource_drift_ignored_attributes )
       ; Aws.Util.option_map v.resource_drift_status (fun f ->
             "ResourceDriftStatus", StackResourceDriftStatus.to_json f)
       ; Some ("Scope", Scope.to_json v.scope)
       ; Aws.Util.option_map v.replacement (fun f -> "Replacement", Replacement.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
       ; Aws.Util.option_map v.physical_resource_id (fun f ->
             "PhysicalResourceId", String.to_json f)
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             "LogicalResourceId", String.to_json f)
       ; Aws.Util.option_map v.action (fun f -> "Action", ChangeAction.to_json f)
       ; Aws.Util.option_map v.policy_action (fun f ->
             "PolicyAction", PolicyAction.to_json f)
       ])

let of_json j =
  { policy_action =
      Aws.Util.option_map (Aws.Json.lookup j "PolicyAction") PolicyAction.of_json
  ; action = Aws.Util.option_map (Aws.Json.lookup j "Action") ChangeAction.of_json
  ; logical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "LogicalResourceId") String.of_json
  ; physical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
  ; resource_type = Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
  ; replacement =
      Aws.Util.option_map (Aws.Json.lookup j "Replacement") Replacement.of_json
  ; scope = Scope.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Scope"))
  ; resource_drift_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResourceDriftStatus")
        StackResourceDriftStatus.of_json
  ; resource_drift_ignored_attributes =
      ResourceDriftIgnoredAttributes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceDriftIgnoredAttributes"))
  ; details =
      ResourceChangeDetails.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Details"))
  ; change_set_id = Aws.Util.option_map (Aws.Json.lookup j "ChangeSetId") String.of_json
  ; module_info = Aws.Util.option_map (Aws.Json.lookup j "ModuleInfo") ModuleInfo.of_json
  ; before_context =
      Aws.Util.option_map (Aws.Json.lookup j "BeforeContext") String.of_json
  ; after_context = Aws.Util.option_map (Aws.Json.lookup j "AfterContext") String.of_json
  ; previous_deployment_context =
      Aws.Util.option_map (Aws.Json.lookup j "PreviousDeploymentContext") String.of_json
  }
