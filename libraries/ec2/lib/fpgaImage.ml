open Aws.BaseTypes

type t =
  { fpga_image_id : String.t option
  ; fpga_image_global_id : String.t option
  ; name : String.t option
  ; description : String.t option
  ; shell_version : String.t option
  ; pci_id : PciId.t option
  ; state : FpgaImageState.t option
  ; create_time : DateTime.t option
  ; update_time : DateTime.t option
  ; owner_id : String.t option
  ; owner_alias : String.t option
  ; product_codes : ProductCodeList.t
  ; tags : TagList.t
  ; public : Boolean.t option
  ; data_retention_support : Boolean.t option
  ; instance_types : InstanceTypesList.t
  }

let make
    ?fpga_image_id
    ?fpga_image_global_id
    ?name
    ?description
    ?shell_version
    ?pci_id
    ?state
    ?create_time
    ?update_time
    ?owner_id
    ?owner_alias
    ?(product_codes = [])
    ?(tags = [])
    ?public
    ?data_retention_support
    ?(instance_types = [])
    () =
  { fpga_image_id
  ; fpga_image_global_id
  ; name
  ; description
  ; shell_version
  ; pci_id
  ; state
  ; create_time
  ; update_time
  ; owner_id
  ; owner_alias
  ; product_codes
  ; tags
  ; public
  ; data_retention_support
  ; instance_types
  }

let parse xml =
  Some
    { fpga_image_id = Aws.Util.option_bind (Aws.Xml.member "fpgaImageId" xml) String.parse
    ; fpga_image_global_id =
        Aws.Util.option_bind (Aws.Xml.member "fpgaImageGlobalId" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; shell_version =
        Aws.Util.option_bind (Aws.Xml.member "shellVersion" xml) String.parse
    ; pci_id = Aws.Util.option_bind (Aws.Xml.member "pciId" xml) PciId.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) FpgaImageState.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; update_time = Aws.Util.option_bind (Aws.Xml.member "updateTime" xml) DateTime.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; owner_alias = Aws.Util.option_bind (Aws.Xml.member "ownerAlias" xml) String.parse
    ; product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "productCodes" xml) ProductCodeList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tags" xml) TagList.parse)
    ; public = Aws.Util.option_bind (Aws.Xml.member "public" xml) Boolean.parse
    ; data_retention_support =
        Aws.Util.option_bind (Aws.Xml.member "dataRetentionSupport" xml) Boolean.parse
    ; instance_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceTypes" xml)
             InstanceTypesList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("InstanceTypes", InstanceTypesList.to_query v.instance_types))
       ; Aws.Util.option_map v.data_retention_support (fun f ->
             Aws.Query.Pair ("DataRetentionSupport", Boolean.to_query f))
       ; Aws.Util.option_map v.public (fun f ->
             Aws.Query.Pair ("Public", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Tags", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("ProductCodes", ProductCodeList.to_query v.product_codes))
       ; Aws.Util.option_map v.owner_alias (fun f ->
             Aws.Query.Pair ("OwnerAlias", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.update_time (fun f ->
             Aws.Query.Pair ("UpdateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", FpgaImageState.to_query f))
       ; Aws.Util.option_map v.pci_id (fun f ->
             Aws.Query.Pair ("PciId", PciId.to_query f))
       ; Aws.Util.option_map v.shell_version (fun f ->
             Aws.Query.Pair ("ShellVersion", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.fpga_image_global_id (fun f ->
             Aws.Query.Pair ("FpgaImageGlobalId", String.to_query f))
       ; Aws.Util.option_map v.fpga_image_id (fun f ->
             Aws.Query.Pair ("FpgaImageId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("instanceTypes", InstanceTypesList.to_json v.instance_types)
       ; Aws.Util.option_map v.data_retention_support (fun f ->
             "dataRetentionSupport", Boolean.to_json f)
       ; Aws.Util.option_map v.public (fun f -> "public", Boolean.to_json f)
       ; Some ("tags", TagList.to_json v.tags)
       ; Some ("productCodes", ProductCodeList.to_json v.product_codes)
       ; Aws.Util.option_map v.owner_alias (fun f -> "ownerAlias", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.update_time (fun f -> "updateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", FpgaImageState.to_json f)
       ; Aws.Util.option_map v.pci_id (fun f -> "pciId", PciId.to_json f)
       ; Aws.Util.option_map v.shell_version (fun f -> "shellVersion", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "name", String.to_json f)
       ; Aws.Util.option_map v.fpga_image_global_id (fun f ->
             "fpgaImageGlobalId", String.to_json f)
       ; Aws.Util.option_map v.fpga_image_id (fun f -> "fpgaImageId", String.to_json f)
       ])

let of_json j =
  { fpga_image_id = Aws.Util.option_map (Aws.Json.lookup j "fpgaImageId") String.of_json
  ; fpga_image_global_id =
      Aws.Util.option_map (Aws.Json.lookup j "fpgaImageGlobalId") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; shell_version = Aws.Util.option_map (Aws.Json.lookup j "shellVersion") String.of_json
  ; pci_id = Aws.Util.option_map (Aws.Json.lookup j "pciId") PciId.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") FpgaImageState.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; update_time = Aws.Util.option_map (Aws.Json.lookup j "updateTime") DateTime.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; owner_alias = Aws.Util.option_map (Aws.Json.lookup j "ownerAlias") String.of_json
  ; product_codes =
      ProductCodeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "productCodes"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tags"))
  ; public = Aws.Util.option_map (Aws.Json.lookup j "public") Boolean.of_json
  ; data_retention_support =
      Aws.Util.option_map (Aws.Json.lookup j "dataRetentionSupport") Boolean.of_json
  ; instance_types =
      InstanceTypesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceTypes"))
  }
