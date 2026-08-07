open Aws.BaseTypes

type t =
  { stack_set_name : String.t option
  ; stack_set_id : String.t option
  ; description : String.t option
  ; status : StackSetStatus.t option
  ; template_body : String.t option
  ; parameters : Parameters.t
  ; capabilities : Capabilities.t
  ; tags : Tags.t
  ; stack_set_a_r_n : String.t option
  ; administration_role_a_r_n : String.t option
  ; execution_role_name : String.t option
  ; stack_set_drift_detection_details : StackSetDriftDetectionDetails.t option
  ; auto_deployment : AutoDeployment.t option
  ; permission_model : PermissionModels.t option
  ; organizational_unit_ids : OrganizationalUnitIdList.t
  ; managed_execution : ManagedExecution.t option
  ; regions : RegionList.t
  }

let make
    ?stack_set_name
    ?stack_set_id
    ?description
    ?status
    ?template_body
    ?(parameters = [])
    ?(capabilities = [])
    ?(tags = [])
    ?stack_set_a_r_n
    ?administration_role_a_r_n
    ?execution_role_name
    ?stack_set_drift_detection_details
    ?auto_deployment
    ?permission_model
    ?(organizational_unit_ids = [])
    ?managed_execution
    ?(regions = [])
    () =
  { stack_set_name
  ; stack_set_id
  ; description
  ; status
  ; template_body
  ; parameters
  ; capabilities
  ; tags
  ; stack_set_a_r_n
  ; administration_role_a_r_n
  ; execution_role_name
  ; stack_set_drift_detection_details
  ; auto_deployment
  ; permission_model
  ; organizational_unit_ids
  ; managed_execution
  ; regions
  }

let parse xml =
  Some
    { stack_set_name =
        Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse
    ; stack_set_id = Aws.Util.option_bind (Aws.Xml.member "StackSetId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackSetStatus.parse
    ; template_body =
        Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
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
    ; stack_set_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "StackSetARN" xml) String.parse
    ; administration_role_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "AdministrationRoleARN" xml) String.parse
    ; execution_role_name =
        Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleName" xml) String.parse
    ; stack_set_drift_detection_details =
        Aws.Util.option_bind
          (Aws.Xml.member "StackSetDriftDetectionDetails" xml)
          StackSetDriftDetectionDetails.parse
    ; auto_deployment =
        Aws.Util.option_bind (Aws.Xml.member "AutoDeployment" xml) AutoDeployment.parse
    ; permission_model =
        Aws.Util.option_bind (Aws.Xml.member "PermissionModel" xml) PermissionModels.parse
    ; organizational_unit_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OrganizationalUnitIds" xml)
             OrganizationalUnitIdList.parse)
    ; managed_execution =
        Aws.Util.option_bind
          (Aws.Xml.member "ManagedExecution" xml)
          ManagedExecution.parse
    ; regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Regions" xml) RegionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Regions.member", RegionList.to_query v.regions))
       ; Aws.Util.option_map v.managed_execution (fun f ->
             Aws.Query.Pair ("ManagedExecution", ManagedExecution.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "OrganizationalUnitIds.member"
              , OrganizationalUnitIdList.to_query v.organizational_unit_ids ))
       ; Aws.Util.option_map v.permission_model (fun f ->
             Aws.Query.Pair ("PermissionModel", PermissionModels.to_query f))
       ; Aws.Util.option_map v.auto_deployment (fun f ->
             Aws.Query.Pair ("AutoDeployment", AutoDeployment.to_query f))
       ; Aws.Util.option_map v.stack_set_drift_detection_details (fun f ->
             Aws.Query.Pair
               ("StackSetDriftDetectionDetails", StackSetDriftDetectionDetails.to_query f))
       ; Aws.Util.option_map v.execution_role_name (fun f ->
             Aws.Query.Pair ("ExecutionRoleName", String.to_query f))
       ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
             Aws.Query.Pair ("AdministrationRoleARN", String.to_query f))
       ; Aws.Util.option_map v.stack_set_a_r_n (fun f ->
             Aws.Query.Pair ("StackSetARN", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
       ; Some
           (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
       ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
       ; Aws.Util.option_map v.template_body (fun f ->
             Aws.Query.Pair ("TemplateBody", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", StackSetStatus.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.stack_set_id (fun f ->
             Aws.Query.Pair ("StackSetId", String.to_query f))
       ; Aws.Util.option_map v.stack_set_name (fun f ->
             Aws.Query.Pair ("StackSetName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Regions", RegionList.to_json v.regions)
       ; Aws.Util.option_map v.managed_execution (fun f ->
             "ManagedExecution", ManagedExecution.to_json f)
       ; Some
           ( "OrganizationalUnitIds"
           , OrganizationalUnitIdList.to_json v.organizational_unit_ids )
       ; Aws.Util.option_map v.permission_model (fun f ->
             "PermissionModel", PermissionModels.to_json f)
       ; Aws.Util.option_map v.auto_deployment (fun f ->
             "AutoDeployment", AutoDeployment.to_json f)
       ; Aws.Util.option_map v.stack_set_drift_detection_details (fun f ->
             "StackSetDriftDetectionDetails", StackSetDriftDetectionDetails.to_json f)
       ; Aws.Util.option_map v.execution_role_name (fun f ->
             "ExecutionRoleName", String.to_json f)
       ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
             "AdministrationRoleARN", String.to_json f)
       ; Aws.Util.option_map v.stack_set_a_r_n (fun f -> "StackSetARN", String.to_json f)
       ; Some ("Tags", Tags.to_json v.tags)
       ; Some ("Capabilities", Capabilities.to_json v.capabilities)
       ; Some ("Parameters", Parameters.to_json v.parameters)
       ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", StackSetStatus.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.stack_set_id (fun f -> "StackSetId", String.to_json f)
       ; Aws.Util.option_map v.stack_set_name (fun f -> "StackSetName", String.to_json f)
       ])

let of_json j =
  { stack_set_name = Aws.Util.option_map (Aws.Json.lookup j "StackSetName") String.of_json
  ; stack_set_id = Aws.Util.option_map (Aws.Json.lookup j "StackSetId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") StackSetStatus.of_json
  ; template_body = Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
  ; parameters =
      Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  ; capabilities =
      Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
  ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; stack_set_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "StackSetARN") String.of_json
  ; administration_role_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "AdministrationRoleARN") String.of_json
  ; execution_role_name =
      Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleName") String.of_json
  ; stack_set_drift_detection_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "StackSetDriftDetectionDetails")
        StackSetDriftDetectionDetails.of_json
  ; auto_deployment =
      Aws.Util.option_map (Aws.Json.lookup j "AutoDeployment") AutoDeployment.of_json
  ; permission_model =
      Aws.Util.option_map (Aws.Json.lookup j "PermissionModel") PermissionModels.of_json
  ; organizational_unit_ids =
      OrganizationalUnitIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OrganizationalUnitIds"))
  ; managed_execution =
      Aws.Util.option_map (Aws.Json.lookup j "ManagedExecution") ManagedExecution.of_json
  ; regions = RegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Regions"))
  }
