open Aws.BaseTypes

type t =
  { block_device_mappings : InstanceBlockDeviceMappingList.t
  ; disable_api_termination : AttributeBooleanValue.t option
  ; ena_support : AttributeBooleanValue.t option
  ; enclave_options : EnclaveOptions.t option
  ; ebs_optimized : AttributeBooleanValue.t option
  ; instance_id : String.t option
  ; instance_initiated_shutdown_behavior : AttributeValue.t option
  ; instance_type : AttributeValue.t option
  ; kernel_id : AttributeValue.t option
  ; product_codes : ProductCodeList.t
  ; ramdisk_id : AttributeValue.t option
  ; root_device_name : AttributeValue.t option
  ; source_dest_check : AttributeBooleanValue.t option
  ; sriov_net_support : AttributeValue.t option
  ; user_data : AttributeValue.t option
  ; disable_api_stop : AttributeBooleanValue.t option
  ; groups : GroupIdentifierList.t
  }

let make
    ?(block_device_mappings = [])
    ?disable_api_termination
    ?ena_support
    ?enclave_options
    ?ebs_optimized
    ?instance_id
    ?instance_initiated_shutdown_behavior
    ?instance_type
    ?kernel_id
    ?(product_codes = [])
    ?ramdisk_id
    ?root_device_name
    ?source_dest_check
    ?sriov_net_support
    ?user_data
    ?disable_api_stop
    ?(groups = [])
    () =
  { block_device_mappings
  ; disable_api_termination
  ; ena_support
  ; enclave_options
  ; ebs_optimized
  ; instance_id
  ; instance_initiated_shutdown_behavior
  ; instance_type
  ; kernel_id
  ; product_codes
  ; ramdisk_id
  ; root_device_name
  ; source_dest_check
  ; sriov_net_support
  ; user_data
  ; disable_api_stop
  ; groups
  }

let parse xml =
  Some
    { block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "blockDeviceMapping" xml)
             InstanceBlockDeviceMappingList.parse)
    ; disable_api_termination =
        Aws.Util.option_bind
          (Aws.Xml.member "disableApiTermination" xml)
          AttributeBooleanValue.parse
    ; ena_support =
        Aws.Util.option_bind (Aws.Xml.member "enaSupport" xml) AttributeBooleanValue.parse
    ; enclave_options =
        Aws.Util.option_bind (Aws.Xml.member "enclaveOptions" xml) EnclaveOptions.parse
    ; ebs_optimized =
        Aws.Util.option_bind
          (Aws.Xml.member "ebsOptimized" xml)
          AttributeBooleanValue.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; instance_initiated_shutdown_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceInitiatedShutdownBehavior" xml)
          AttributeValue.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) AttributeValue.parse
    ; kernel_id = Aws.Util.option_bind (Aws.Xml.member "kernel" xml) AttributeValue.parse
    ; product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "productCodes" xml) ProductCodeList.parse)
    ; ramdisk_id =
        Aws.Util.option_bind (Aws.Xml.member "ramdisk" xml) AttributeValue.parse
    ; root_device_name =
        Aws.Util.option_bind (Aws.Xml.member "rootDeviceName" xml) AttributeValue.parse
    ; source_dest_check =
        Aws.Util.option_bind
          (Aws.Xml.member "sourceDestCheck" xml)
          AttributeBooleanValue.parse
    ; sriov_net_support =
        Aws.Util.option_bind (Aws.Xml.member "sriovNetSupport" xml) AttributeValue.parse
    ; user_data =
        Aws.Util.option_bind (Aws.Xml.member "userData" xml) AttributeValue.parse
    ; disable_api_stop =
        Aws.Util.option_bind
          (Aws.Xml.member "disableApiStop" xml)
          AttributeBooleanValue.parse
    ; groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groupSet" xml) GroupIdentifierList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("GroupSet", GroupIdentifierList.to_query v.groups))
       ; Aws.Util.option_map v.disable_api_stop (fun f ->
             Aws.Query.Pair ("DisableApiStop", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.user_data (fun f ->
             Aws.Query.Pair ("UserData", AttributeValue.to_query f))
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             Aws.Query.Pair ("SriovNetSupport", AttributeValue.to_query f))
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             Aws.Query.Pair ("SourceDestCheck", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.root_device_name (fun f ->
             Aws.Query.Pair ("RootDeviceName", AttributeValue.to_query f))
       ; Aws.Util.option_map v.ramdisk_id (fun f ->
             Aws.Query.Pair ("Ramdisk", AttributeValue.to_query f))
       ; Some (Aws.Query.Pair ("ProductCodes", ProductCodeList.to_query v.product_codes))
       ; Aws.Util.option_map v.kernel_id (fun f ->
             Aws.Query.Pair ("Kernel", AttributeValue.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", AttributeValue.to_query f))
       ; Aws.Util.option_map v.instance_initiated_shutdown_behavior (fun f ->
             Aws.Query.Pair
               ("InstanceInitiatedShutdownBehavior", AttributeValue.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.enclave_options (fun f ->
             Aws.Query.Pair ("EnclaveOptions", EnclaveOptions.to_query f))
       ; Aws.Util.option_map v.ena_support (fun f ->
             Aws.Query.Pair ("EnaSupport", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.disable_api_termination (fun f ->
             Aws.Query.Pair ("DisableApiTermination", AttributeBooleanValue.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , InstanceBlockDeviceMappingList.to_query v.block_device_mappings ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("groupSet", GroupIdentifierList.to_json v.groups)
       ; Aws.Util.option_map v.disable_api_stop (fun f ->
             "disableApiStop", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.user_data (fun f -> "userData", AttributeValue.to_json f)
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             "sriovNetSupport", AttributeValue.to_json f)
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             "sourceDestCheck", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.root_device_name (fun f ->
             "rootDeviceName", AttributeValue.to_json f)
       ; Aws.Util.option_map v.ramdisk_id (fun f -> "ramdisk", AttributeValue.to_json f)
       ; Some ("productCodes", ProductCodeList.to_json v.product_codes)
       ; Aws.Util.option_map v.kernel_id (fun f -> "kernel", AttributeValue.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", AttributeValue.to_json f)
       ; Aws.Util.option_map v.instance_initiated_shutdown_behavior (fun f ->
             "instanceInitiatedShutdownBehavior", AttributeValue.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             "ebsOptimized", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.enclave_options (fun f ->
             "enclaveOptions", EnclaveOptions.to_json f)
       ; Aws.Util.option_map v.ena_support (fun f ->
             "enaSupport", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.disable_api_termination (fun f ->
             "disableApiTermination", AttributeBooleanValue.to_json f)
       ; Some
           ( "blockDeviceMapping"
           , InstanceBlockDeviceMappingList.to_json v.block_device_mappings )
       ])

let of_json j =
  { block_device_mappings =
      InstanceBlockDeviceMappingList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "blockDeviceMapping"))
  ; disable_api_termination =
      Aws.Util.option_map
        (Aws.Json.lookup j "disableApiTermination")
        AttributeBooleanValue.of_json
  ; ena_support =
      Aws.Util.option_map (Aws.Json.lookup j "enaSupport") AttributeBooleanValue.of_json
  ; enclave_options =
      Aws.Util.option_map (Aws.Json.lookup j "enclaveOptions") EnclaveOptions.of_json
  ; ebs_optimized =
      Aws.Util.option_map (Aws.Json.lookup j "ebsOptimized") AttributeBooleanValue.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; instance_initiated_shutdown_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceInitiatedShutdownBehavior")
        AttributeValue.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") AttributeValue.of_json
  ; kernel_id = Aws.Util.option_map (Aws.Json.lookup j "kernel") AttributeValue.of_json
  ; product_codes =
      ProductCodeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "productCodes"))
  ; ramdisk_id = Aws.Util.option_map (Aws.Json.lookup j "ramdisk") AttributeValue.of_json
  ; root_device_name =
      Aws.Util.option_map (Aws.Json.lookup j "rootDeviceName") AttributeValue.of_json
  ; source_dest_check =
      Aws.Util.option_map
        (Aws.Json.lookup j "sourceDestCheck")
        AttributeBooleanValue.of_json
  ; sriov_net_support =
      Aws.Util.option_map (Aws.Json.lookup j "sriovNetSupport") AttributeValue.of_json
  ; user_data = Aws.Util.option_map (Aws.Json.lookup j "userData") AttributeValue.of_json
  ; disable_api_stop =
      Aws.Util.option_map
        (Aws.Json.lookup j "disableApiStop")
        AttributeBooleanValue.of_json
  ; groups =
      GroupIdentifierList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupSet"))
  }
