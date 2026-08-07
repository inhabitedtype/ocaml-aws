open Aws.BaseTypes

type t =
  { attribute : String.t option
  ; description : AttributeValue.t option
  ; image_id : String.t
  ; launch_permission : LaunchPermissionModifications.t option
  ; operation_type : OperationType.t option
  ; product_codes : ProductCodeStringList.t
  ; user_groups : UserGroupStringList.t
  ; user_ids : UserIdStringList.t
  ; value : String.t option
  ; organization_arns : OrganizationArnStringList.t
  ; organizational_unit_arns : OrganizationalUnitArnStringList.t
  ; imds_support : AttributeValue.t option
  ; dry_run : Boolean.t option
  }

let make
    ?attribute
    ?description
    ~image_id
    ?launch_permission
    ?operation_type
    ?(product_codes = [])
    ?(user_groups = [])
    ?(user_ids = [])
    ?value
    ?(organization_arns = [])
    ?(organizational_unit_arns = [])
    ?imds_support
    ?dry_run
    () =
  { attribute
  ; description
  ; image_id
  ; launch_permission
  ; operation_type
  ; product_codes
  ; user_groups
  ; user_ids
  ; value
  ; organization_arns
  ; organizational_unit_arns
  ; imds_support
  ; dry_run
  }

let parse xml =
  Some
    { attribute = Aws.Util.option_bind (Aws.Xml.member "Attribute" xml) String.parse
    ; description =
        Aws.Util.option_bind (Aws.Xml.member "Description" xml) AttributeValue.parse
    ; image_id =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse)
    ; launch_permission =
        Aws.Util.option_bind
          (Aws.Xml.member "LaunchPermission" xml)
          LaunchPermissionModifications.parse
    ; operation_type =
        Aws.Util.option_bind (Aws.Xml.member "OperationType" xml) OperationType.parse
    ; product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ProductCode" xml)
             ProductCodeStringList.parse)
    ; user_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UserGroup" xml)
             UserGroupStringList.parse)
    ; user_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "UserId" xml) UserIdStringList.parse)
    ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
    ; organization_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OrganizationArn" xml)
             OrganizationArnStringList.parse)
    ; organizational_unit_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OrganizationalUnitArn" xml)
             OrganizationalUnitArnStringList.parse)
    ; imds_support =
        Aws.Util.option_bind (Aws.Xml.member "ImdsSupport" xml) AttributeValue.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.imds_support (fun f ->
             Aws.Query.Pair ("ImdsSupport", AttributeValue.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "OrganizationalUnitArn"
              , OrganizationalUnitArnStringList.to_query v.organizational_unit_arns ))
       ; Some
           (Aws.Query.Pair
              ("OrganizationArn", OrganizationArnStringList.to_query v.organization_arns))
       ; Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", String.to_query f))
       ; Some (Aws.Query.Pair ("UserId", UserIdStringList.to_query v.user_ids))
       ; Some (Aws.Query.Pair ("UserGroup", UserGroupStringList.to_query v.user_groups))
       ; Some
           (Aws.Query.Pair ("ProductCode", ProductCodeStringList.to_query v.product_codes))
       ; Aws.Util.option_map v.operation_type (fun f ->
             Aws.Query.Pair ("OperationType", OperationType.to_query f))
       ; Aws.Util.option_map v.launch_permission (fun f ->
             Aws.Query.Pair ("LaunchPermission", LaunchPermissionModifications.to_query f))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", AttributeValue.to_query f))
       ; Aws.Util.option_map v.attribute (fun f ->
             Aws.Query.Pair ("Attribute", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.imds_support (fun f ->
             "ImdsSupport", AttributeValue.to_json f)
       ; Some
           ( "OrganizationalUnitArn"
           , OrganizationalUnitArnStringList.to_json v.organizational_unit_arns )
       ; Some ("OrganizationArn", OrganizationArnStringList.to_json v.organization_arns)
       ; Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
       ; Some ("UserId", UserIdStringList.to_json v.user_ids)
       ; Some ("UserGroup", UserGroupStringList.to_json v.user_groups)
       ; Some ("ProductCode", ProductCodeStringList.to_json v.product_codes)
       ; Aws.Util.option_map v.operation_type (fun f ->
             "OperationType", OperationType.to_json f)
       ; Aws.Util.option_map v.launch_permission (fun f ->
             "LaunchPermission", LaunchPermissionModifications.to_json f)
       ; Some ("ImageId", String.to_json v.image_id)
       ; Aws.Util.option_map v.description (fun f ->
             "Description", AttributeValue.to_json f)
       ; Aws.Util.option_map v.attribute (fun f -> "Attribute", String.to_json f)
       ])

let of_json j =
  { attribute = Aws.Util.option_map (Aws.Json.lookup j "Attribute") String.of_json
  ; description =
      Aws.Util.option_map (Aws.Json.lookup j "Description") AttributeValue.of_json
  ; image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; launch_permission =
      Aws.Util.option_map
        (Aws.Json.lookup j "LaunchPermission")
        LaunchPermissionModifications.of_json
  ; operation_type =
      Aws.Util.option_map (Aws.Json.lookup j "OperationType") OperationType.of_json
  ; product_codes =
      ProductCodeStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ProductCode"))
  ; user_groups =
      UserGroupStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroup"))
  ; user_ids =
      UserIdStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserId"))
  ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
  ; organization_arns =
      OrganizationArnStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OrganizationArn"))
  ; organizational_unit_arns =
      OrganizationalUnitArnStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OrganizationalUnitArn"))
  ; imds_support =
      Aws.Util.option_map (Aws.Json.lookup j "ImdsSupport") AttributeValue.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
