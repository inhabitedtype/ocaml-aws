open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; fpga_image_id : String.t
  ; attribute : FpgaImageAttributeName.t option
  ; operation_type : OperationType.t option
  ; user_ids : UserIdStringList.t
  ; user_groups : UserGroupStringList.t
  ; product_codes : ProductCodeStringList.t
  ; load_permission : LoadPermissionModifications.t option
  ; description : String.t option
  ; name : String.t option
  }

let make
    ?dry_run
    ~fpga_image_id
    ?attribute
    ?operation_type
    ?(user_ids = [])
    ?(user_groups = [])
    ?(product_codes = [])
    ?load_permission
    ?description
    ?name
    () =
  { dry_run
  ; fpga_image_id
  ; attribute
  ; operation_type
  ; user_ids
  ; user_groups
  ; product_codes
  ; load_permission
  ; description
  ; name
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; fpga_image_id =
        Aws.Xml.required
          "FpgaImageId"
          (Aws.Util.option_bind (Aws.Xml.member "FpgaImageId" xml) String.parse)
    ; attribute =
        Aws.Util.option_bind (Aws.Xml.member "Attribute" xml) FpgaImageAttributeName.parse
    ; operation_type =
        Aws.Util.option_bind (Aws.Xml.member "OperationType" xml) OperationType.parse
    ; user_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "UserId" xml) UserIdStringList.parse)
    ; user_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UserGroup" xml)
             UserGroupStringList.parse)
    ; product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ProductCode" xml)
             ProductCodeStringList.parse)
    ; load_permission =
        Aws.Util.option_bind
          (Aws.Xml.member "LoadPermission" xml)
          LoadPermissionModifications.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.load_permission (fun f ->
             Aws.Query.Pair ("LoadPermission", LoadPermissionModifications.to_query f))
       ; Some
           (Aws.Query.Pair ("ProductCode", ProductCodeStringList.to_query v.product_codes))
       ; Some (Aws.Query.Pair ("UserGroup", UserGroupStringList.to_query v.user_groups))
       ; Some (Aws.Query.Pair ("UserId", UserIdStringList.to_query v.user_ids))
       ; Aws.Util.option_map v.operation_type (fun f ->
             Aws.Query.Pair ("OperationType", OperationType.to_query f))
       ; Aws.Util.option_map v.attribute (fun f ->
             Aws.Query.Pair ("Attribute", FpgaImageAttributeName.to_query f))
       ; Some (Aws.Query.Pair ("FpgaImageId", String.to_query v.fpga_image_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.load_permission (fun f ->
             "LoadPermission", LoadPermissionModifications.to_json f)
       ; Some ("ProductCode", ProductCodeStringList.to_json v.product_codes)
       ; Some ("UserGroup", UserGroupStringList.to_json v.user_groups)
       ; Some ("UserId", UserIdStringList.to_json v.user_ids)
       ; Aws.Util.option_map v.operation_type (fun f ->
             "OperationType", OperationType.to_json f)
       ; Aws.Util.option_map v.attribute (fun f ->
             "Attribute", FpgaImageAttributeName.to_json f)
       ; Some ("FpgaImageId", String.to_json v.fpga_image_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; fpga_image_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FpgaImageId"))
  ; attribute =
      Aws.Util.option_map (Aws.Json.lookup j "Attribute") FpgaImageAttributeName.of_json
  ; operation_type =
      Aws.Util.option_map (Aws.Json.lookup j "OperationType") OperationType.of_json
  ; user_ids =
      UserIdStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserId"))
  ; user_groups =
      UserGroupStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroup"))
  ; product_codes =
      ProductCodeStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ProductCode"))
  ; load_permission =
      Aws.Util.option_map
        (Aws.Json.lookup j "LoadPermission")
        LoadPermissionModifications.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  }
