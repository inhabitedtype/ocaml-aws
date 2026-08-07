open Aws.BaseTypes

type t =
  { blue_green_deployment_name : String.t
  ; source : String.t
  ; target_engine_version : String.t option
  ; target_d_b_parameter_group_name : String.t option
  ; target_d_b_cluster_parameter_group_name : String.t option
  ; tags : TagList.t
  ; target_d_b_instance_class : String.t option
  ; upgrade_target_storage_config : Boolean.t option
  ; target_iops : Integer.t option
  ; target_storage_type : String.t option
  ; target_allocated_storage : Integer.t option
  ; target_storage_throughput : Integer.t option
  }

let make
    ~blue_green_deployment_name
    ~source
    ?target_engine_version
    ?target_d_b_parameter_group_name
    ?target_d_b_cluster_parameter_group_name
    ?(tags = [])
    ?target_d_b_instance_class
    ?upgrade_target_storage_config
    ?target_iops
    ?target_storage_type
    ?target_allocated_storage
    ?target_storage_throughput
    () =
  { blue_green_deployment_name
  ; source
  ; target_engine_version
  ; target_d_b_parameter_group_name
  ; target_d_b_cluster_parameter_group_name
  ; tags
  ; target_d_b_instance_class
  ; upgrade_target_storage_config
  ; target_iops
  ; target_storage_type
  ; target_allocated_storage
  ; target_storage_throughput
  }

let parse xml =
  Some
    { blue_green_deployment_name =
        Aws.Xml.required
          "BlueGreenDeploymentName"
          (Aws.Util.option_bind
             (Aws.Xml.member "BlueGreenDeploymentName" xml)
             String.parse)
    ; source =
        Aws.Xml.required
          "Source"
          (Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse)
    ; target_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "TargetEngineVersion" xml) String.parse
    ; target_d_b_parameter_group_name =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetDBParameterGroupName" xml)
          String.parse
    ; target_d_b_cluster_parameter_group_name =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetDBClusterParameterGroupName" xml)
          String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; target_d_b_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "TargetDBInstanceClass" xml) String.parse
    ; upgrade_target_storage_config =
        Aws.Util.option_bind
          (Aws.Xml.member "UpgradeTargetStorageConfig" xml)
          Boolean.parse
    ; target_iops = Aws.Util.option_bind (Aws.Xml.member "TargetIops" xml) Integer.parse
    ; target_storage_type =
        Aws.Util.option_bind (Aws.Xml.member "TargetStorageType" xml) String.parse
    ; target_allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "TargetAllocatedStorage" xml) Integer.parse
    ; target_storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "TargetStorageThroughput" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_storage_throughput (fun f ->
             Aws.Query.Pair ("TargetStorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.target_allocated_storage (fun f ->
             Aws.Query.Pair ("TargetAllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.target_storage_type (fun f ->
             Aws.Query.Pair ("TargetStorageType", String.to_query f))
       ; Aws.Util.option_map v.target_iops (fun f ->
             Aws.Query.Pair ("TargetIops", Integer.to_query f))
       ; Aws.Util.option_map v.upgrade_target_storage_config (fun f ->
             Aws.Query.Pair ("UpgradeTargetStorageConfig", Boolean.to_query f))
       ; Aws.Util.option_map v.target_d_b_instance_class (fun f ->
             Aws.Query.Pair ("TargetDBInstanceClass", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.target_d_b_cluster_parameter_group_name (fun f ->
             Aws.Query.Pair ("TargetDBClusterParameterGroupName", String.to_query f))
       ; Aws.Util.option_map v.target_d_b_parameter_group_name (fun f ->
             Aws.Query.Pair ("TargetDBParameterGroupName", String.to_query f))
       ; Aws.Util.option_map v.target_engine_version (fun f ->
             Aws.Query.Pair ("TargetEngineVersion", String.to_query f))
       ; Some (Aws.Query.Pair ("Source", String.to_query v.source))
       ; Some
           (Aws.Query.Pair
              ("BlueGreenDeploymentName", String.to_query v.blue_green_deployment_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_storage_throughput (fun f ->
             "TargetStorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.target_allocated_storage (fun f ->
             "TargetAllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.target_storage_type (fun f ->
             "TargetStorageType", String.to_json f)
       ; Aws.Util.option_map v.target_iops (fun f -> "TargetIops", Integer.to_json f)
       ; Aws.Util.option_map v.upgrade_target_storage_config (fun f ->
             "UpgradeTargetStorageConfig", Boolean.to_json f)
       ; Aws.Util.option_map v.target_d_b_instance_class (fun f ->
             "TargetDBInstanceClass", String.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.target_d_b_cluster_parameter_group_name (fun f ->
             "TargetDBClusterParameterGroupName", String.to_json f)
       ; Aws.Util.option_map v.target_d_b_parameter_group_name (fun f ->
             "TargetDBParameterGroupName", String.to_json f)
       ; Aws.Util.option_map v.target_engine_version (fun f ->
             "TargetEngineVersion", String.to_json f)
       ; Some ("Source", String.to_json v.source)
       ; Some ("BlueGreenDeploymentName", String.to_json v.blue_green_deployment_name)
       ])

let of_json j =
  { blue_green_deployment_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BlueGreenDeploymentName"))
  ; source = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Source"))
  ; target_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "TargetEngineVersion") String.of_json
  ; target_d_b_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "TargetDBParameterGroupName") String.of_json
  ; target_d_b_cluster_parameter_group_name =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetDBClusterParameterGroupName")
        String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; target_d_b_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "TargetDBInstanceClass") String.of_json
  ; upgrade_target_storage_config =
      Aws.Util.option_map (Aws.Json.lookup j "UpgradeTargetStorageConfig") Boolean.of_json
  ; target_iops = Aws.Util.option_map (Aws.Json.lookup j "TargetIops") Integer.of_json
  ; target_storage_type =
      Aws.Util.option_map (Aws.Json.lookup j "TargetStorageType") String.of_json
  ; target_allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "TargetAllocatedStorage") Integer.of_json
  ; target_storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "TargetStorageThroughput") Integer.of_json
  }
