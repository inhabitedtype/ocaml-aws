open Aws.BaseTypes

type t =
  { source_dest_check : AttributeBooleanValue.t option
  ; enclave_options : EnclaveOptionsRequest.t option
  ; disable_api_stop : AttributeBooleanValue.t option
  ; dry_run : Boolean.t option
  ; instance_id : String.t
  ; attribute : InstanceAttributeName.t option
  ; value : String.t option
  ; block_device_mappings : InstanceBlockDeviceMappingSpecificationList.t
  ; disable_api_termination : AttributeBooleanValue.t option
  ; instance_type : AttributeValue.t option
  ; kernel : AttributeValue.t option
  ; ramdisk : AttributeValue.t option
  ; user_data : SecureBlobAttributeValue.t option
  ; instance_initiated_shutdown_behavior : AttributeValue.t option
  ; groups : GroupIdStringList.t
  ; ebs_optimized : AttributeBooleanValue.t option
  ; sriov_net_support : AttributeValue.t option
  ; ena_support : AttributeBooleanValue.t option
  }

let make
    ?source_dest_check
    ?enclave_options
    ?disable_api_stop
    ?dry_run
    ~instance_id
    ?attribute
    ?value
    ?(block_device_mappings = [])
    ?disable_api_termination
    ?instance_type
    ?kernel
    ?ramdisk
    ?user_data
    ?instance_initiated_shutdown_behavior
    ?(groups = [])
    ?ebs_optimized
    ?sriov_net_support
    ?ena_support
    () =
  { source_dest_check
  ; enclave_options
  ; disable_api_stop
  ; dry_run
  ; instance_id
  ; attribute
  ; value
  ; block_device_mappings
  ; disable_api_termination
  ; instance_type
  ; kernel
  ; ramdisk
  ; user_data
  ; instance_initiated_shutdown_behavior
  ; groups
  ; ebs_optimized
  ; sriov_net_support
  ; ena_support
  }

let parse xml =
  Some
    { source_dest_check =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceDestCheck" xml)
          AttributeBooleanValue.parse
    ; enclave_options =
        Aws.Util.option_bind
          (Aws.Xml.member "EnclaveOptions" xml)
          EnclaveOptionsRequest.parse
    ; disable_api_stop =
        Aws.Util.option_bind
          (Aws.Xml.member "DisableApiStop" xml)
          AttributeBooleanValue.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; instance_id =
        Aws.Xml.required
          "instanceId"
          (Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse)
    ; attribute =
        Aws.Util.option_bind (Aws.Xml.member "attribute" xml) InstanceAttributeName.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "value" xml) String.parse
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "blockDeviceMapping" xml)
             InstanceBlockDeviceMappingSpecificationList.parse)
    ; disable_api_termination =
        Aws.Util.option_bind
          (Aws.Xml.member "disableApiTermination" xml)
          AttributeBooleanValue.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) AttributeValue.parse
    ; kernel = Aws.Util.option_bind (Aws.Xml.member "kernel" xml) AttributeValue.parse
    ; ramdisk = Aws.Util.option_bind (Aws.Xml.member "ramdisk" xml) AttributeValue.parse
    ; user_data =
        Aws.Util.option_bind
          (Aws.Xml.member "userData" xml)
          SecureBlobAttributeValue.parse
    ; instance_initiated_shutdown_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceInitiatedShutdownBehavior" xml)
          AttributeValue.parse
    ; groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "GroupId" xml) GroupIdStringList.parse)
    ; ebs_optimized =
        Aws.Util.option_bind
          (Aws.Xml.member "ebsOptimized" xml)
          AttributeBooleanValue.parse
    ; sriov_net_support =
        Aws.Util.option_bind (Aws.Xml.member "sriovNetSupport" xml) AttributeValue.parse
    ; ena_support =
        Aws.Util.option_bind (Aws.Xml.member "enaSupport" xml) AttributeBooleanValue.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_support (fun f ->
             Aws.Query.Pair ("EnaSupport", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             Aws.Query.Pair ("SriovNetSupport", AttributeValue.to_query f))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", AttributeBooleanValue.to_query f))
       ; Some (Aws.Query.Pair ("GroupId", GroupIdStringList.to_query v.groups))
       ; Aws.Util.option_map v.instance_initiated_shutdown_behavior (fun f ->
             Aws.Query.Pair
               ("InstanceInitiatedShutdownBehavior", AttributeValue.to_query f))
       ; Aws.Util.option_map v.user_data (fun f ->
             Aws.Query.Pair ("UserData", SecureBlobAttributeValue.to_query f))
       ; Aws.Util.option_map v.ramdisk (fun f ->
             Aws.Query.Pair ("Ramdisk", AttributeValue.to_query f))
       ; Aws.Util.option_map v.kernel (fun f ->
             Aws.Query.Pair ("Kernel", AttributeValue.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", AttributeValue.to_query f))
       ; Aws.Util.option_map v.disable_api_termination (fun f ->
             Aws.Query.Pair ("DisableApiTermination", AttributeBooleanValue.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , InstanceBlockDeviceMappingSpecificationList.to_query
                  v.block_device_mappings ))
       ; Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", String.to_query f))
       ; Aws.Util.option_map v.attribute (fun f ->
             Aws.Query.Pair ("Attribute", InstanceAttributeName.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.disable_api_stop (fun f ->
             Aws.Query.Pair ("DisableApiStop", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.enclave_options (fun f ->
             Aws.Query.Pair ("EnclaveOptions", EnclaveOptionsRequest.to_query f))
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             Aws.Query.Pair ("SourceDestCheck", AttributeBooleanValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ena_support (fun f ->
             "enaSupport", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             "sriovNetSupport", AttributeValue.to_json f)
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             "ebsOptimized", AttributeBooleanValue.to_json f)
       ; Some ("GroupId", GroupIdStringList.to_json v.groups)
       ; Aws.Util.option_map v.instance_initiated_shutdown_behavior (fun f ->
             "instanceInitiatedShutdownBehavior", AttributeValue.to_json f)
       ; Aws.Util.option_map v.user_data (fun f ->
             "userData", SecureBlobAttributeValue.to_json f)
       ; Aws.Util.option_map v.ramdisk (fun f -> "ramdisk", AttributeValue.to_json f)
       ; Aws.Util.option_map v.kernel (fun f -> "kernel", AttributeValue.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", AttributeValue.to_json f)
       ; Aws.Util.option_map v.disable_api_termination (fun f ->
             "disableApiTermination", AttributeBooleanValue.to_json f)
       ; Some
           ( "blockDeviceMapping"
           , InstanceBlockDeviceMappingSpecificationList.to_json v.block_device_mappings
           )
       ; Aws.Util.option_map v.value (fun f -> "value", String.to_json f)
       ; Aws.Util.option_map v.attribute (fun f ->
             "attribute", InstanceAttributeName.to_json f)
       ; Some ("instanceId", String.to_json v.instance_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.disable_api_stop (fun f ->
             "DisableApiStop", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.enclave_options (fun f ->
             "EnclaveOptions", EnclaveOptionsRequest.to_json f)
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             "SourceDestCheck", AttributeBooleanValue.to_json f)
       ])

let of_json j =
  { source_dest_check =
      Aws.Util.option_map
        (Aws.Json.lookup j "SourceDestCheck")
        AttributeBooleanValue.of_json
  ; enclave_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnclaveOptions")
        EnclaveOptionsRequest.of_json
  ; disable_api_stop =
      Aws.Util.option_map
        (Aws.Json.lookup j "DisableApiStop")
        AttributeBooleanValue.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceId"))
  ; attribute =
      Aws.Util.option_map (Aws.Json.lookup j "attribute") InstanceAttributeName.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "value") String.of_json
  ; block_device_mappings =
      InstanceBlockDeviceMappingSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "blockDeviceMapping"))
  ; disable_api_termination =
      Aws.Util.option_map
        (Aws.Json.lookup j "disableApiTermination")
        AttributeBooleanValue.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") AttributeValue.of_json
  ; kernel = Aws.Util.option_map (Aws.Json.lookup j "kernel") AttributeValue.of_json
  ; ramdisk = Aws.Util.option_map (Aws.Json.lookup j "ramdisk") AttributeValue.of_json
  ; user_data =
      Aws.Util.option_map (Aws.Json.lookup j "userData") SecureBlobAttributeValue.of_json
  ; instance_initiated_shutdown_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceInitiatedShutdownBehavior")
        AttributeValue.of_json
  ; groups =
      GroupIdStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupId"))
  ; ebs_optimized =
      Aws.Util.option_map (Aws.Json.lookup j "ebsOptimized") AttributeBooleanValue.of_json
  ; sriov_net_support =
      Aws.Util.option_map (Aws.Json.lookup j "sriovNetSupport") AttributeValue.of_json
  ; ena_support =
      Aws.Util.option_map (Aws.Json.lookup j "enaSupport") AttributeBooleanValue.of_json
  }
