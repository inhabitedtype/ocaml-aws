open Aws.BaseTypes

type t =
  { stack_name : String.t
  ; template_body : String.t option
  ; template_u_r_l : String.t option
  ; use_previous_template : Boolean.t option
  ; parameters : Parameters.t
  ; capabilities : Capabilities.t
  ; resource_types : ResourceTypes.t
  ; role_a_r_n : String.t option
  ; rollback_configuration : RollbackConfiguration.t option
  ; notification_a_r_ns : NotificationARNs.t
  ; tags : Tags.t
  ; change_set_name : String.t
  ; client_token : String.t option
  ; description : String.t option
  ; change_set_type : ChangeSetType.t option
  ; resources_to_import : ResourcesToImport.t
  ; include_nested_stacks : Boolean.t option
  ; on_stack_failure : OnStackFailure.t option
  ; import_existing_resources : Boolean.t option
  ; deployment_mode : DeploymentMode.t option
  }

let make
    ~stack_name
    ?template_body
    ?template_u_r_l
    ?use_previous_template
    ?(parameters = [])
    ?(capabilities = [])
    ?(resource_types = [])
    ?role_a_r_n
    ?rollback_configuration
    ?(notification_a_r_ns = [])
    ?(tags = [])
    ~change_set_name
    ?client_token
    ?description
    ?change_set_type
    ?(resources_to_import = [])
    ?include_nested_stacks
    ?on_stack_failure
    ?import_existing_resources
    ?deployment_mode
    () =
  { stack_name
  ; template_body
  ; template_u_r_l
  ; use_previous_template
  ; parameters
  ; capabilities
  ; resource_types
  ; role_a_r_n
  ; rollback_configuration
  ; notification_a_r_ns
  ; tags
  ; change_set_name
  ; client_token
  ; description
  ; change_set_type
  ; resources_to_import
  ; include_nested_stacks
  ; on_stack_failure
  ; import_existing_resources
  ; deployment_mode
  }

let parse xml =
  Some
    { stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
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
    ; resource_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ResourceTypes" xml) ResourceTypes.parse)
    ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
    ; rollback_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "RollbackConfiguration" xml)
          RollbackConfiguration.parse
    ; notification_a_r_ns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NotificationARNs" xml)
             NotificationARNs.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
    ; change_set_name =
        Aws.Xml.required
          "ChangeSetName"
          (Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; change_set_type =
        Aws.Util.option_bind (Aws.Xml.member "ChangeSetType" xml) ChangeSetType.parse
    ; resources_to_import =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourcesToImport" xml)
             ResourcesToImport.parse)
    ; include_nested_stacks =
        Aws.Util.option_bind (Aws.Xml.member "IncludeNestedStacks" xml) Boolean.parse
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
       ; Aws.Util.option_map v.include_nested_stacks (fun f ->
             Aws.Query.Pair ("IncludeNestedStacks", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ResourcesToImport.member"
              , ResourcesToImport.to_query v.resources_to_import ))
       ; Aws.Util.option_map v.change_set_type (fun f ->
             Aws.Query.Pair ("ChangeSetType", ChangeSetType.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some (Aws.Query.Pair ("ChangeSetName", String.to_query v.change_set_name))
       ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ("NotificationARNs.member", NotificationARNs.to_query v.notification_a_r_ns))
       ; Aws.Util.option_map v.rollback_configuration (fun f ->
             Aws.Query.Pair ("RollbackConfiguration", RollbackConfiguration.to_query f))
       ; Aws.Util.option_map v.role_a_r_n (fun f ->
             Aws.Query.Pair ("RoleARN", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ResourceTypes.member", ResourceTypes.to_query v.resource_types))
       ; Some
           (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
       ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
       ; Aws.Util.option_map v.use_previous_template (fun f ->
             Aws.Query.Pair ("UsePreviousTemplate", Boolean.to_query f))
       ; Aws.Util.option_map v.template_u_r_l (fun f ->
             Aws.Query.Pair ("TemplateURL", String.to_query f))
       ; Aws.Util.option_map v.template_body (fun f ->
             Aws.Query.Pair ("TemplateBody", String.to_query f))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
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
       ; Aws.Util.option_map v.include_nested_stacks (fun f ->
             "IncludeNestedStacks", Boolean.to_json f)
       ; Some ("ResourcesToImport", ResourcesToImport.to_json v.resources_to_import)
       ; Aws.Util.option_map v.change_set_type (fun f ->
             "ChangeSetType", ChangeSetType.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("ChangeSetName", String.to_json v.change_set_name)
       ; Some ("Tags", Tags.to_json v.tags)
       ; Some ("NotificationARNs", NotificationARNs.to_json v.notification_a_r_ns)
       ; Aws.Util.option_map v.rollback_configuration (fun f ->
             "RollbackConfiguration", RollbackConfiguration.to_json f)
       ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
       ; Some ("ResourceTypes", ResourceTypes.to_json v.resource_types)
       ; Some ("Capabilities", Capabilities.to_json v.capabilities)
       ; Some ("Parameters", Parameters.to_json v.parameters)
       ; Aws.Util.option_map v.use_previous_template (fun f ->
             "UsePreviousTemplate", Boolean.to_json f)
       ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
       ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
       ; Some ("StackName", String.to_json v.stack_name)
       ])

let of_json j =
  { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; template_body = Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
  ; template_u_r_l = Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
  ; use_previous_template =
      Aws.Util.option_map (Aws.Json.lookup j "UsePreviousTemplate") Boolean.of_json
  ; parameters =
      Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  ; capabilities =
      Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
  ; resource_types =
      ResourceTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTypes"))
  ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
  ; rollback_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "RollbackConfiguration")
        RollbackConfiguration.of_json
  ; notification_a_r_ns =
      NotificationARNs.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationARNs"))
  ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; change_set_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeSetName"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; change_set_type =
      Aws.Util.option_map (Aws.Json.lookup j "ChangeSetType") ChangeSetType.of_json
  ; resources_to_import =
      ResourcesToImport.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourcesToImport"))
  ; include_nested_stacks =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeNestedStacks") Boolean.of_json
  ; on_stack_failure =
      Aws.Util.option_map (Aws.Json.lookup j "OnStackFailure") OnStackFailure.of_json
  ; import_existing_resources =
      Aws.Util.option_map (Aws.Json.lookup j "ImportExistingResources") Boolean.of_json
  ; deployment_mode =
      Aws.Util.option_map (Aws.Json.lookup j "DeploymentMode") DeploymentMode.of_json
  }
