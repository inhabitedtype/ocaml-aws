open Aws.BaseTypes

type t =
  { stack_set_name : String.t
  ; description : String.t option
  ; template_body : String.t option
  ; template_u_r_l : String.t option
  ; use_previous_template : Boolean.t option
  ; parameters : Parameters.t
  ; capabilities : Capabilities.t
  ; tags : Tags.t
  ; operation_preferences : StackSetOperationPreferences.t option
  ; administration_role_a_r_n : String.t option
  ; execution_role_name : String.t option
  ; deployment_targets : DeploymentTargets.t option
  ; permission_model : PermissionModels.t option
  ; auto_deployment : AutoDeployment.t option
  ; operation_id : String.t option
  ; accounts : AccountList.t
  ; regions : RegionList.t
  ; call_as : CallAs.t option
  ; managed_execution : ManagedExecution.t option
  }

let make
    ~stack_set_name
    ?description
    ?template_body
    ?template_u_r_l
    ?use_previous_template
    ?(parameters = [])
    ?(capabilities = [])
    ?(tags = [])
    ?operation_preferences
    ?administration_role_a_r_n
    ?execution_role_name
    ?deployment_targets
    ?permission_model
    ?auto_deployment
    ?operation_id
    ?(accounts = [])
    ?(regions = [])
    ?call_as
    ?managed_execution
    () =
  { stack_set_name
  ; description
  ; template_body
  ; template_u_r_l
  ; use_previous_template
  ; parameters
  ; capabilities
  ; tags
  ; operation_preferences
  ; administration_role_a_r_n
  ; execution_role_name
  ; deployment_targets
  ; permission_model
  ; auto_deployment
  ; operation_id
  ; accounts
  ; regions
  ; call_as
  ; managed_execution
  }

let parse xml =
  Some
    { stack_set_name =
        Aws.Xml.required
          "StackSetName"
          (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; template_body =
        Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
    ; template_u_r_l =
        Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
    ; use_previous_template =
        Aws.Util.option_bind (Aws.Xml.member "UsePreviousTemplate" xml) Boolean.parse
    ; parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
    ; capabilities =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
    ; operation_preferences =
        Aws.Util.option_bind
          (Aws.Xml.member "OperationPreferences" xml)
          StackSetOperationPreferences.parse
    ; administration_role_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "AdministrationRoleARN" xml) String.parse
    ; execution_role_name =
        Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleName" xml) String.parse
    ; deployment_targets =
        Aws.Util.option_bind
          (Aws.Xml.member "DeploymentTargets" xml)
          DeploymentTargets.parse
    ; permission_model =
        Aws.Util.option_bind (Aws.Xml.member "PermissionModel" xml) PermissionModels.parse
    ; auto_deployment =
        Aws.Util.option_bind (Aws.Xml.member "AutoDeployment" xml) AutoDeployment.parse
    ; operation_id = Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
    ; accounts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Accounts" xml) AccountList.parse)
    ; regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Regions" xml) RegionList.parse)
    ; call_as = Aws.Util.option_bind (Aws.Xml.member "CallAs" xml) CallAs.parse
    ; managed_execution =
        Aws.Util.option_bind
          (Aws.Xml.member "ManagedExecution" xml)
          ManagedExecution.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_execution (fun f ->
             Aws.Query.Pair ("ManagedExecution", ManagedExecution.to_query f))
       ; Aws.Util.option_map v.call_as (fun f ->
             Aws.Query.Pair ("CallAs", CallAs.to_query f))
       ; Some (Aws.Query.Pair ("Regions.member", RegionList.to_query v.regions))
       ; Some (Aws.Query.Pair ("Accounts.member", AccountList.to_query v.accounts))
       ; Aws.Util.option_map v.operation_id (fun f ->
             Aws.Query.Pair ("OperationId", String.to_query f))
       ; Aws.Util.option_map v.auto_deployment (fun f ->
             Aws.Query.Pair ("AutoDeployment", AutoDeployment.to_query f))
       ; Aws.Util.option_map v.permission_model (fun f ->
             Aws.Query.Pair ("PermissionModel", PermissionModels.to_query f))
       ; Aws.Util.option_map v.deployment_targets (fun f ->
             Aws.Query.Pair ("DeploymentTargets", DeploymentTargets.to_query f))
       ; Aws.Util.option_map v.execution_role_name (fun f ->
             Aws.Query.Pair ("ExecutionRoleName", String.to_query f))
       ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
             Aws.Query.Pair ("AdministrationRoleARN", String.to_query f))
       ; Aws.Util.option_map v.operation_preferences (fun f ->
             Aws.Query.Pair
               ("OperationPreferences", StackSetOperationPreferences.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
       ; Some
           (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
       ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
       ; Aws.Util.option_map v.use_previous_template (fun f ->
             Aws.Query.Pair ("UsePreviousTemplate", Boolean.to_query f))
       ; Aws.Util.option_map v.template_u_r_l (fun f ->
             Aws.Query.Pair ("TemplateURL", String.to_query f))
       ; Aws.Util.option_map v.template_body (fun f ->
             Aws.Query.Pair ("TemplateBody", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_execution (fun f ->
             "ManagedExecution", ManagedExecution.to_json f)
       ; Aws.Util.option_map v.call_as (fun f -> "CallAs", CallAs.to_json f)
       ; Some ("Regions", RegionList.to_json v.regions)
       ; Some ("Accounts", AccountList.to_json v.accounts)
       ; Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
       ; Aws.Util.option_map v.auto_deployment (fun f ->
             "AutoDeployment", AutoDeployment.to_json f)
       ; Aws.Util.option_map v.permission_model (fun f ->
             "PermissionModel", PermissionModels.to_json f)
       ; Aws.Util.option_map v.deployment_targets (fun f ->
             "DeploymentTargets", DeploymentTargets.to_json f)
       ; Aws.Util.option_map v.execution_role_name (fun f ->
             "ExecutionRoleName", String.to_json f)
       ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
             "AdministrationRoleARN", String.to_json f)
       ; Aws.Util.option_map v.operation_preferences (fun f ->
             "OperationPreferences", StackSetOperationPreferences.to_json f)
       ; Some ("Tags", Tags.to_json v.tags)
       ; Some ("Capabilities", Capabilities.to_json v.capabilities)
       ; Some ("Parameters", Parameters.to_json v.parameters)
       ; Aws.Util.option_map v.use_previous_template (fun f ->
             "UsePreviousTemplate", Boolean.to_json f)
       ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
       ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("StackSetName", String.to_json v.stack_set_name)
       ])

let of_json j =
  { stack_set_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; template_body = Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
  ; template_u_r_l = Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
  ; use_previous_template =
      Aws.Util.option_map (Aws.Json.lookup j "UsePreviousTemplate") Boolean.of_json
  ; parameters =
      Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  ; capabilities =
      Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
  ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; operation_preferences =
      Aws.Util.option_map
        (Aws.Json.lookup j "OperationPreferences")
        StackSetOperationPreferences.of_json
  ; administration_role_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "AdministrationRoleARN") String.of_json
  ; execution_role_name =
      Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleName") String.of_json
  ; deployment_targets =
      Aws.Util.option_map
        (Aws.Json.lookup j "DeploymentTargets")
        DeploymentTargets.of_json
  ; permission_model =
      Aws.Util.option_map (Aws.Json.lookup j "PermissionModel") PermissionModels.of_json
  ; auto_deployment =
      Aws.Util.option_map (Aws.Json.lookup j "AutoDeployment") AutoDeployment.of_json
  ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
  ; accounts = AccountList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Accounts"))
  ; regions = RegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Regions"))
  ; call_as = Aws.Util.option_map (Aws.Json.lookup j "CallAs") CallAs.of_json
  ; managed_execution =
      Aws.Util.option_map (Aws.Json.lookup j "ManagedExecution") ManagedExecution.of_json
  }
