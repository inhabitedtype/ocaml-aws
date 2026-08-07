open Aws.BaseTypes

type t =
  { stack_name : String.t
  ; template_body : String.t option
  ; template_u_r_l : String.t option
  ; parameters : Parameters.t
  ; disable_rollback : Boolean.t option
  ; rollback_configuration : RollbackConfiguration.t option
  ; timeout_in_minutes : Integer.t option
  ; notification_a_r_ns : NotificationARNs.t
  ; capabilities : Capabilities.t
  ; resource_types : ResourceTypes.t
  ; role_a_r_n : String.t option
  ; on_failure : OnFailure.t option
  ; stack_policy_body : String.t option
  ; stack_policy_u_r_l : String.t option
  ; tags : Tags.t
  ; client_request_token : String.t option
  ; enable_termination_protection : Boolean.t option
  ; retain_except_on_create : Boolean.t option
  }

let make
    ~stack_name
    ?template_body
    ?template_u_r_l
    ?(parameters = [])
    ?disable_rollback
    ?rollback_configuration
    ?timeout_in_minutes
    ?(notification_a_r_ns = [])
    ?(capabilities = [])
    ?(resource_types = [])
    ?role_a_r_n
    ?on_failure
    ?stack_policy_body
    ?stack_policy_u_r_l
    ?(tags = [])
    ?client_request_token
    ?enable_termination_protection
    ?retain_except_on_create
    () =
  { stack_name
  ; template_body
  ; template_u_r_l
  ; parameters
  ; disable_rollback
  ; rollback_configuration
  ; timeout_in_minutes
  ; notification_a_r_ns
  ; capabilities
  ; resource_types
  ; role_a_r_n
  ; on_failure
  ; stack_policy_body
  ; stack_policy_u_r_l
  ; tags
  ; client_request_token
  ; enable_termination_protection
  ; retain_except_on_create
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
    ; parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
    ; disable_rollback =
        Aws.Util.option_bind (Aws.Xml.member "DisableRollback" xml) Boolean.parse
    ; rollback_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "RollbackConfiguration" xml)
          RollbackConfiguration.parse
    ; timeout_in_minutes =
        Aws.Util.option_bind (Aws.Xml.member "TimeoutInMinutes" xml) Integer.parse
    ; notification_a_r_ns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NotificationARNs" xml)
             NotificationARNs.parse)
    ; capabilities =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
    ; resource_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ResourceTypes" xml) ResourceTypes.parse)
    ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
    ; on_failure = Aws.Util.option_bind (Aws.Xml.member "OnFailure" xml) OnFailure.parse
    ; stack_policy_body =
        Aws.Util.option_bind (Aws.Xml.member "StackPolicyBody" xml) String.parse
    ; stack_policy_u_r_l =
        Aws.Util.option_bind (Aws.Xml.member "StackPolicyURL" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
    ; client_request_token =
        Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
    ; enable_termination_protection =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableTerminationProtection" xml)
          Boolean.parse
    ; retain_except_on_create =
        Aws.Util.option_bind (Aws.Xml.member "RetainExceptOnCreate" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retain_except_on_create (fun f ->
             Aws.Query.Pair ("RetainExceptOnCreate", Boolean.to_query f))
       ; Aws.Util.option_map v.enable_termination_protection (fun f ->
             Aws.Query.Pair ("EnableTerminationProtection", Boolean.to_query f))
       ; Aws.Util.option_map v.client_request_token (fun f ->
             Aws.Query.Pair ("ClientRequestToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
       ; Aws.Util.option_map v.stack_policy_u_r_l (fun f ->
             Aws.Query.Pair ("StackPolicyURL", String.to_query f))
       ; Aws.Util.option_map v.stack_policy_body (fun f ->
             Aws.Query.Pair ("StackPolicyBody", String.to_query f))
       ; Aws.Util.option_map v.on_failure (fun f ->
             Aws.Query.Pair ("OnFailure", OnFailure.to_query f))
       ; Aws.Util.option_map v.role_a_r_n (fun f ->
             Aws.Query.Pair ("RoleARN", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ResourceTypes.member", ResourceTypes.to_query v.resource_types))
       ; Some
           (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
       ; Some
           (Aws.Query.Pair
              ("NotificationARNs.member", NotificationARNs.to_query v.notification_a_r_ns))
       ; Aws.Util.option_map v.timeout_in_minutes (fun f ->
             Aws.Query.Pair ("TimeoutInMinutes", Integer.to_query f))
       ; Aws.Util.option_map v.rollback_configuration (fun f ->
             Aws.Query.Pair ("RollbackConfiguration", RollbackConfiguration.to_query f))
       ; Aws.Util.option_map v.disable_rollback (fun f ->
             Aws.Query.Pair ("DisableRollback", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
       ; Aws.Util.option_map v.template_u_r_l (fun f ->
             Aws.Query.Pair ("TemplateURL", String.to_query f))
       ; Aws.Util.option_map v.template_body (fun f ->
             Aws.Query.Pair ("TemplateBody", String.to_query f))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retain_except_on_create (fun f ->
             "RetainExceptOnCreate", Boolean.to_json f)
       ; Aws.Util.option_map v.enable_termination_protection (fun f ->
             "EnableTerminationProtection", Boolean.to_json f)
       ; Aws.Util.option_map v.client_request_token (fun f ->
             "ClientRequestToken", String.to_json f)
       ; Some ("Tags", Tags.to_json v.tags)
       ; Aws.Util.option_map v.stack_policy_u_r_l (fun f ->
             "StackPolicyURL", String.to_json f)
       ; Aws.Util.option_map v.stack_policy_body (fun f ->
             "StackPolicyBody", String.to_json f)
       ; Aws.Util.option_map v.on_failure (fun f -> "OnFailure", OnFailure.to_json f)
       ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
       ; Some ("ResourceTypes", ResourceTypes.to_json v.resource_types)
       ; Some ("Capabilities", Capabilities.to_json v.capabilities)
       ; Some ("NotificationARNs", NotificationARNs.to_json v.notification_a_r_ns)
       ; Aws.Util.option_map v.timeout_in_minutes (fun f ->
             "TimeoutInMinutes", Integer.to_json f)
       ; Aws.Util.option_map v.rollback_configuration (fun f ->
             "RollbackConfiguration", RollbackConfiguration.to_json f)
       ; Aws.Util.option_map v.disable_rollback (fun f ->
             "DisableRollback", Boolean.to_json f)
       ; Some ("Parameters", Parameters.to_json v.parameters)
       ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
       ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
       ; Some ("StackName", String.to_json v.stack_name)
       ])

let of_json j =
  { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; template_body = Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
  ; template_u_r_l = Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
  ; parameters =
      Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  ; disable_rollback =
      Aws.Util.option_map (Aws.Json.lookup j "DisableRollback") Boolean.of_json
  ; rollback_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "RollbackConfiguration")
        RollbackConfiguration.of_json
  ; timeout_in_minutes =
      Aws.Util.option_map (Aws.Json.lookup j "TimeoutInMinutes") Integer.of_json
  ; notification_a_r_ns =
      NotificationARNs.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationARNs"))
  ; capabilities =
      Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
  ; resource_types =
      ResourceTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTypes"))
  ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
  ; on_failure = Aws.Util.option_map (Aws.Json.lookup j "OnFailure") OnFailure.of_json
  ; stack_policy_body =
      Aws.Util.option_map (Aws.Json.lookup j "StackPolicyBody") String.of_json
  ; stack_policy_u_r_l =
      Aws.Util.option_map (Aws.Json.lookup j "StackPolicyURL") String.of_json
  ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; client_request_token =
      Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
  ; enable_termination_protection =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableTerminationProtection")
        Boolean.of_json
  ; retain_except_on_create =
      Aws.Util.option_map (Aws.Json.lookup j "RetainExceptOnCreate") Boolean.of_json
  }
