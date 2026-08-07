open Aws.BaseTypes

type t =
  { stack_id : String.t option
  ; stack_name : String.t
  ; change_set_id : String.t option
  ; description : String.t option
  ; parameters : Parameters.t
  ; creation_time : DateTime.t
  ; deletion_time : DateTime.t option
  ; last_updated_time : DateTime.t option
  ; rollback_configuration : RollbackConfiguration.t option
  ; stack_status : StackStatus.t
  ; stack_status_reason : String.t option
  ; disable_rollback : Boolean.t option
  ; notification_a_r_ns : NotificationARNs.t
  ; timeout_in_minutes : Integer.t option
  ; capabilities : Capabilities.t
  ; outputs : Outputs.t
  ; role_a_r_n : String.t option
  ; tags : Tags.t
  ; enable_termination_protection : Boolean.t option
  ; parent_id : String.t option
  ; root_id : String.t option
  ; drift_information : StackDriftInformation.t option
  ; retain_except_on_create : Boolean.t option
  ; deletion_mode : DeletionMode.t option
  ; detailed_status : DetailedStatus.t option
  ; last_operations : LastOperations.t
  }

let make
    ?stack_id
    ~stack_name
    ?change_set_id
    ?description
    ?(parameters = [])
    ~creation_time
    ?deletion_time
    ?last_updated_time
    ?rollback_configuration
    ~stack_status
    ?stack_status_reason
    ?disable_rollback
    ?(notification_a_r_ns = [])
    ?timeout_in_minutes
    ?(capabilities = [])
    ?(outputs = [])
    ?role_a_r_n
    ?(tags = [])
    ?enable_termination_protection
    ?parent_id
    ?root_id
    ?drift_information
    ?retain_except_on_create
    ?deletion_mode
    ?detailed_status
    ?(last_operations = [])
    () =
  { stack_id
  ; stack_name
  ; change_set_id
  ; description
  ; parameters
  ; creation_time
  ; deletion_time
  ; last_updated_time
  ; rollback_configuration
  ; stack_status
  ; stack_status_reason
  ; disable_rollback
  ; notification_a_r_ns
  ; timeout_in_minutes
  ; capabilities
  ; outputs
  ; role_a_r_n
  ; tags
  ; enable_termination_protection
  ; parent_id
  ; root_id
  ; drift_information
  ; retain_except_on_create
  ; deletion_mode
  ; detailed_status
  ; last_operations
  }

let parse xml =
  Some
    { stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
    ; stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    ; change_set_id = Aws.Util.option_bind (Aws.Xml.member "ChangeSetId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
    ; creation_time =
        Aws.Xml.required
          "CreationTime"
          (Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse)
    ; deletion_time =
        Aws.Util.option_bind (Aws.Xml.member "DeletionTime" xml) DateTime.parse
    ; last_updated_time =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdatedTime" xml) DateTime.parse
    ; rollback_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "RollbackConfiguration" xml)
          RollbackConfiguration.parse
    ; stack_status =
        Aws.Xml.required
          "StackStatus"
          (Aws.Util.option_bind (Aws.Xml.member "StackStatus" xml) StackStatus.parse)
    ; stack_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StackStatusReason" xml) String.parse
    ; disable_rollback =
        Aws.Util.option_bind (Aws.Xml.member "DisableRollback" xml) Boolean.parse
    ; notification_a_r_ns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NotificationARNs" xml)
             NotificationARNs.parse)
    ; timeout_in_minutes =
        Aws.Util.option_bind (Aws.Xml.member "TimeoutInMinutes" xml) Integer.parse
    ; capabilities =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
    ; outputs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Outputs" xml) Outputs.parse)
    ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
    ; enable_termination_protection =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableTerminationProtection" xml)
          Boolean.parse
    ; parent_id = Aws.Util.option_bind (Aws.Xml.member "ParentId" xml) String.parse
    ; root_id = Aws.Util.option_bind (Aws.Xml.member "RootId" xml) String.parse
    ; drift_information =
        Aws.Util.option_bind
          (Aws.Xml.member "DriftInformation" xml)
          StackDriftInformation.parse
    ; retain_except_on_create =
        Aws.Util.option_bind (Aws.Xml.member "RetainExceptOnCreate" xml) Boolean.parse
    ; deletion_mode =
        Aws.Util.option_bind (Aws.Xml.member "DeletionMode" xml) DeletionMode.parse
    ; detailed_status =
        Aws.Util.option_bind (Aws.Xml.member "DetailedStatus" xml) DetailedStatus.parse
    ; last_operations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LastOperations" xml)
             LastOperations.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("LastOperations.member", LastOperations.to_query v.last_operations))
       ; Aws.Util.option_map v.detailed_status (fun f ->
             Aws.Query.Pair ("DetailedStatus", DetailedStatus.to_query f))
       ; Aws.Util.option_map v.deletion_mode (fun f ->
             Aws.Query.Pair ("DeletionMode", DeletionMode.to_query f))
       ; Aws.Util.option_map v.retain_except_on_create (fun f ->
             Aws.Query.Pair ("RetainExceptOnCreate", Boolean.to_query f))
       ; Aws.Util.option_map v.drift_information (fun f ->
             Aws.Query.Pair ("DriftInformation", StackDriftInformation.to_query f))
       ; Aws.Util.option_map v.root_id (fun f ->
             Aws.Query.Pair ("RootId", String.to_query f))
       ; Aws.Util.option_map v.parent_id (fun f ->
             Aws.Query.Pair ("ParentId", String.to_query f))
       ; Aws.Util.option_map v.enable_termination_protection (fun f ->
             Aws.Query.Pair ("EnableTerminationProtection", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
       ; Aws.Util.option_map v.role_a_r_n (fun f ->
             Aws.Query.Pair ("RoleARN", String.to_query f))
       ; Some (Aws.Query.Pair ("Outputs.member", Outputs.to_query v.outputs))
       ; Some
           (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
       ; Aws.Util.option_map v.timeout_in_minutes (fun f ->
             Aws.Query.Pair ("TimeoutInMinutes", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("NotificationARNs.member", NotificationARNs.to_query v.notification_a_r_ns))
       ; Aws.Util.option_map v.disable_rollback (fun f ->
             Aws.Query.Pair ("DisableRollback", Boolean.to_query f))
       ; Aws.Util.option_map v.stack_status_reason (fun f ->
             Aws.Query.Pair ("StackStatusReason", String.to_query f))
       ; Some (Aws.Query.Pair ("StackStatus", StackStatus.to_query v.stack_status))
       ; Aws.Util.option_map v.rollback_configuration (fun f ->
             Aws.Query.Pair ("RollbackConfiguration", RollbackConfiguration.to_query f))
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             Aws.Query.Pair ("LastUpdatedTime", DateTime.to_query f))
       ; Aws.Util.option_map v.deletion_time (fun f ->
             Aws.Query.Pair ("DeletionTime", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("CreationTime", DateTime.to_query v.creation_time))
       ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.change_set_id (fun f ->
             Aws.Query.Pair ("ChangeSetId", String.to_query f))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ; Aws.Util.option_map v.stack_id (fun f ->
             Aws.Query.Pair ("StackId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LastOperations", LastOperations.to_json v.last_operations)
       ; Aws.Util.option_map v.detailed_status (fun f ->
             "DetailedStatus", DetailedStatus.to_json f)
       ; Aws.Util.option_map v.deletion_mode (fun f ->
             "DeletionMode", DeletionMode.to_json f)
       ; Aws.Util.option_map v.retain_except_on_create (fun f ->
             "RetainExceptOnCreate", Boolean.to_json f)
       ; Aws.Util.option_map v.drift_information (fun f ->
             "DriftInformation", StackDriftInformation.to_json f)
       ; Aws.Util.option_map v.root_id (fun f -> "RootId", String.to_json f)
       ; Aws.Util.option_map v.parent_id (fun f -> "ParentId", String.to_json f)
       ; Aws.Util.option_map v.enable_termination_protection (fun f ->
             "EnableTerminationProtection", Boolean.to_json f)
       ; Some ("Tags", Tags.to_json v.tags)
       ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
       ; Some ("Outputs", Outputs.to_json v.outputs)
       ; Some ("Capabilities", Capabilities.to_json v.capabilities)
       ; Aws.Util.option_map v.timeout_in_minutes (fun f ->
             "TimeoutInMinutes", Integer.to_json f)
       ; Some ("NotificationARNs", NotificationARNs.to_json v.notification_a_r_ns)
       ; Aws.Util.option_map v.disable_rollback (fun f ->
             "DisableRollback", Boolean.to_json f)
       ; Aws.Util.option_map v.stack_status_reason (fun f ->
             "StackStatusReason", String.to_json f)
       ; Some ("StackStatus", StackStatus.to_json v.stack_status)
       ; Aws.Util.option_map v.rollback_configuration (fun f ->
             "RollbackConfiguration", RollbackConfiguration.to_json f)
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             "LastUpdatedTime", DateTime.to_json f)
       ; Aws.Util.option_map v.deletion_time (fun f -> "DeletionTime", DateTime.to_json f)
       ; Some ("CreationTime", DateTime.to_json v.creation_time)
       ; Some ("Parameters", Parameters.to_json v.parameters)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.change_set_id (fun f -> "ChangeSetId", String.to_json f)
       ; Some ("StackName", String.to_json v.stack_name)
       ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
       ])

let of_json j =
  { stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
  ; stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; change_set_id = Aws.Util.option_map (Aws.Json.lookup j "ChangeSetId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; parameters =
      Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  ; creation_time =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CreationTime"))
  ; deletion_time =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionTime") DateTime.of_json
  ; last_updated_time =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedTime") DateTime.of_json
  ; rollback_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "RollbackConfiguration")
        RollbackConfiguration.of_json
  ; stack_status =
      StackStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackStatus"))
  ; stack_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "StackStatusReason") String.of_json
  ; disable_rollback =
      Aws.Util.option_map (Aws.Json.lookup j "DisableRollback") Boolean.of_json
  ; notification_a_r_ns =
      NotificationARNs.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationARNs"))
  ; timeout_in_minutes =
      Aws.Util.option_map (Aws.Json.lookup j "TimeoutInMinutes") Integer.of_json
  ; capabilities =
      Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
  ; outputs = Outputs.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Outputs"))
  ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
  ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; enable_termination_protection =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableTerminationProtection")
        Boolean.of_json
  ; parent_id = Aws.Util.option_map (Aws.Json.lookup j "ParentId") String.of_json
  ; root_id = Aws.Util.option_map (Aws.Json.lookup j "RootId") String.of_json
  ; drift_information =
      Aws.Util.option_map
        (Aws.Json.lookup j "DriftInformation")
        StackDriftInformation.of_json
  ; retain_except_on_create =
      Aws.Util.option_map (Aws.Json.lookup j "RetainExceptOnCreate") Boolean.of_json
  ; deletion_mode =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionMode") DeletionMode.of_json
  ; detailed_status =
      Aws.Util.option_map (Aws.Json.lookup j "DetailedStatus") DetailedStatus.of_json
  ; last_operations =
      LastOperations.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LastOperations"))
  }
