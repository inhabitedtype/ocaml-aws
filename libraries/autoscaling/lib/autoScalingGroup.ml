open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; auto_scaling_group_a_r_n : String.t option
  ; launch_configuration_name : String.t option
  ; launch_template : LaunchTemplateSpecification.t option
  ; mixed_instances_policy : MixedInstancesPolicy.t option
  ; min_size : Integer.t
  ; max_size : Integer.t
  ; desired_capacity : Integer.t
  ; predicted_capacity : Integer.t option
  ; default_cooldown : Integer.t
  ; availability_zones : AvailabilityZones.t
  ; availability_zone_ids : AvailabilityZoneIds.t
  ; load_balancer_names : LoadBalancerNames.t
  ; target_group_a_r_ns : TargetGroupARNs.t
  ; health_check_type : String.t
  ; health_check_grace_period : Integer.t option
  ; instances : Instances.t
  ; created_time : DateTime.t
  ; suspended_processes : SuspendedProcesses.t
  ; placement_group : String.t option
  ; v_p_c_zone_identifier : String.t option
  ; enabled_metrics : EnabledMetrics.t
  ; status : String.t option
  ; tags : TagDescriptionList.t
  ; termination_policies : TerminationPolicies.t
  ; new_instances_protected_from_scale_in : Boolean.t option
  ; service_linked_role_a_r_n : String.t option
  ; max_instance_lifetime : Integer.t option
  ; capacity_rebalance : Boolean.t option
  ; warm_pool_configuration : WarmPoolConfiguration.t option
  ; warm_pool_size : Integer.t option
  ; context : String.t option
  ; desired_capacity_type : String.t option
  ; default_instance_warmup : Integer.t option
  ; traffic_sources : TrafficSources.t
  ; instance_maintenance_policy : InstanceMaintenancePolicy.t option
  ; deletion_protection : DeletionProtection.t option
  ; availability_zone_distribution : AvailabilityZoneDistribution.t option
  ; availability_zone_impairment_policy : AvailabilityZoneImpairmentPolicy.t option
  ; capacity_reservation_specification : CapacityReservationSpecification.t option
  ; instance_lifecycle_policy : InstanceLifecyclePolicy.t option
  }

let make
    ~auto_scaling_group_name
    ?auto_scaling_group_a_r_n
    ?launch_configuration_name
    ?launch_template
    ?mixed_instances_policy
    ~min_size
    ~max_size
    ~desired_capacity
    ?predicted_capacity
    ~default_cooldown
    ~availability_zones
    ?(availability_zone_ids = [])
    ?(load_balancer_names = [])
    ?(target_group_a_r_ns = [])
    ~health_check_type
    ?health_check_grace_period
    ?(instances = [])
    ~created_time
    ?(suspended_processes = [])
    ?placement_group
    ?v_p_c_zone_identifier
    ?(enabled_metrics = [])
    ?status
    ?(tags = [])
    ?(termination_policies = [])
    ?new_instances_protected_from_scale_in
    ?service_linked_role_a_r_n
    ?max_instance_lifetime
    ?capacity_rebalance
    ?warm_pool_configuration
    ?warm_pool_size
    ?context
    ?desired_capacity_type
    ?default_instance_warmup
    ?(traffic_sources = [])
    ?instance_maintenance_policy
    ?deletion_protection
    ?availability_zone_distribution
    ?availability_zone_impairment_policy
    ?capacity_reservation_specification
    ?instance_lifecycle_policy
    () =
  { auto_scaling_group_name
  ; auto_scaling_group_a_r_n
  ; launch_configuration_name
  ; launch_template
  ; mixed_instances_policy
  ; min_size
  ; max_size
  ; desired_capacity
  ; predicted_capacity
  ; default_cooldown
  ; availability_zones
  ; availability_zone_ids
  ; load_balancer_names
  ; target_group_a_r_ns
  ; health_check_type
  ; health_check_grace_period
  ; instances
  ; created_time
  ; suspended_processes
  ; placement_group
  ; v_p_c_zone_identifier
  ; enabled_metrics
  ; status
  ; tags
  ; termination_policies
  ; new_instances_protected_from_scale_in
  ; service_linked_role_a_r_n
  ; max_instance_lifetime
  ; capacity_rebalance
  ; warm_pool_configuration
  ; warm_pool_size
  ; context
  ; desired_capacity_type
  ; default_instance_warmup
  ; traffic_sources
  ; instance_maintenance_policy
  ; deletion_protection
  ; availability_zone_distribution
  ; availability_zone_impairment_policy
  ; capacity_reservation_specification
  ; instance_lifecycle_policy
  }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; auto_scaling_group_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupARN" xml) String.parse
    ; launch_configuration_name =
        Aws.Util.option_bind (Aws.Xml.member "LaunchConfigurationName" xml) String.parse
    ; launch_template =
        Aws.Util.option_bind
          (Aws.Xml.member "LaunchTemplate" xml)
          LaunchTemplateSpecification.parse
    ; mixed_instances_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "MixedInstancesPolicy" xml)
          MixedInstancesPolicy.parse
    ; min_size =
        Aws.Xml.required
          "MinSize"
          (Aws.Util.option_bind (Aws.Xml.member "MinSize" xml) Integer.parse)
    ; max_size =
        Aws.Xml.required
          "MaxSize"
          (Aws.Util.option_bind (Aws.Xml.member "MaxSize" xml) Integer.parse)
    ; desired_capacity =
        Aws.Xml.required
          "DesiredCapacity"
          (Aws.Util.option_bind (Aws.Xml.member "DesiredCapacity" xml) Integer.parse)
    ; predicted_capacity =
        Aws.Util.option_bind (Aws.Xml.member "PredictedCapacity" xml) Integer.parse
    ; default_cooldown =
        Aws.Xml.required
          "DefaultCooldown"
          (Aws.Util.option_bind (Aws.Xml.member "DefaultCooldown" xml) Integer.parse)
    ; availability_zones =
        Aws.Xml.required
          "AvailabilityZones"
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZones" xml)
             AvailabilityZones.parse)
    ; availability_zone_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZoneIds" xml)
             AvailabilityZoneIds.parse)
    ; load_balancer_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LoadBalancerNames" xml)
             LoadBalancerNames.parse)
    ; target_group_a_r_ns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetGroupARNs" xml)
             TargetGroupARNs.parse)
    ; health_check_type =
        Aws.Xml.required
          "HealthCheckType"
          (Aws.Util.option_bind (Aws.Xml.member "HealthCheckType" xml) String.parse)
    ; health_check_grace_period =
        Aws.Util.option_bind (Aws.Xml.member "HealthCheckGracePeriod" xml) Integer.parse
    ; instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
    ; created_time =
        Aws.Xml.required
          "CreatedTime"
          (Aws.Util.option_bind (Aws.Xml.member "CreatedTime" xml) DateTime.parse)
    ; suspended_processes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SuspendedProcesses" xml)
             SuspendedProcesses.parse)
    ; placement_group =
        Aws.Util.option_bind (Aws.Xml.member "PlacementGroup" xml) String.parse
    ; v_p_c_zone_identifier =
        Aws.Util.option_bind (Aws.Xml.member "VPCZoneIdentifier" xml) String.parse
    ; enabled_metrics =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EnabledMetrics" xml)
             EnabledMetrics.parse)
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagDescriptionList.parse)
    ; termination_policies =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TerminationPolicies" xml)
             TerminationPolicies.parse)
    ; new_instances_protected_from_scale_in =
        Aws.Util.option_bind
          (Aws.Xml.member "NewInstancesProtectedFromScaleIn" xml)
          Boolean.parse
    ; service_linked_role_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "ServiceLinkedRoleARN" xml) String.parse
    ; max_instance_lifetime =
        Aws.Util.option_bind (Aws.Xml.member "MaxInstanceLifetime" xml) Integer.parse
    ; capacity_rebalance =
        Aws.Util.option_bind (Aws.Xml.member "CapacityRebalance" xml) Boolean.parse
    ; warm_pool_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "WarmPoolConfiguration" xml)
          WarmPoolConfiguration.parse
    ; warm_pool_size =
        Aws.Util.option_bind (Aws.Xml.member "WarmPoolSize" xml) Integer.parse
    ; context = Aws.Util.option_bind (Aws.Xml.member "Context" xml) String.parse
    ; desired_capacity_type =
        Aws.Util.option_bind (Aws.Xml.member "DesiredCapacityType" xml) String.parse
    ; default_instance_warmup =
        Aws.Util.option_bind (Aws.Xml.member "DefaultInstanceWarmup" xml) Integer.parse
    ; traffic_sources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficSources" xml)
             TrafficSources.parse)
    ; instance_maintenance_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceMaintenancePolicy" xml)
          InstanceMaintenancePolicy.parse
    ; deletion_protection =
        Aws.Util.option_bind
          (Aws.Xml.member "DeletionProtection" xml)
          DeletionProtection.parse
    ; availability_zone_distribution =
        Aws.Util.option_bind
          (Aws.Xml.member "AvailabilityZoneDistribution" xml)
          AvailabilityZoneDistribution.parse
    ; availability_zone_impairment_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "AvailabilityZoneImpairmentPolicy" xml)
          AvailabilityZoneImpairmentPolicy.parse
    ; capacity_reservation_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "CapacityReservationSpecification" xml)
          CapacityReservationSpecification.parse
    ; instance_lifecycle_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceLifecyclePolicy" xml)
          InstanceLifecyclePolicy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_lifecycle_policy (fun f ->
             Aws.Query.Pair ("InstanceLifecyclePolicy", InstanceLifecyclePolicy.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_specification (fun f ->
             Aws.Query.Pair
               ( "CapacityReservationSpecification"
               , CapacityReservationSpecification.to_query f ))
       ; Aws.Util.option_map v.availability_zone_impairment_policy (fun f ->
             Aws.Query.Pair
               ( "AvailabilityZoneImpairmentPolicy"
               , AvailabilityZoneImpairmentPolicy.to_query f ))
       ; Aws.Util.option_map v.availability_zone_distribution (fun f ->
             Aws.Query.Pair
               ("AvailabilityZoneDistribution", AvailabilityZoneDistribution.to_query f))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", DeletionProtection.to_query f))
       ; Aws.Util.option_map v.instance_maintenance_policy (fun f ->
             Aws.Query.Pair
               ("InstanceMaintenancePolicy", InstanceMaintenancePolicy.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TrafficSources.member", TrafficSources.to_query v.traffic_sources))
       ; Aws.Util.option_map v.default_instance_warmup (fun f ->
             Aws.Query.Pair ("DefaultInstanceWarmup", Integer.to_query f))
       ; Aws.Util.option_map v.desired_capacity_type (fun f ->
             Aws.Query.Pair ("DesiredCapacityType", String.to_query f))
       ; Aws.Util.option_map v.context (fun f ->
             Aws.Query.Pair ("Context", String.to_query f))
       ; Aws.Util.option_map v.warm_pool_size (fun f ->
             Aws.Query.Pair ("WarmPoolSize", Integer.to_query f))
       ; Aws.Util.option_map v.warm_pool_configuration (fun f ->
             Aws.Query.Pair ("WarmPoolConfiguration", WarmPoolConfiguration.to_query f))
       ; Aws.Util.option_map v.capacity_rebalance (fun f ->
             Aws.Query.Pair ("CapacityRebalance", Boolean.to_query f))
       ; Aws.Util.option_map v.max_instance_lifetime (fun f ->
             Aws.Query.Pair ("MaxInstanceLifetime", Integer.to_query f))
       ; Aws.Util.option_map v.service_linked_role_a_r_n (fun f ->
             Aws.Query.Pair ("ServiceLinkedRoleARN", String.to_query f))
       ; Aws.Util.option_map v.new_instances_protected_from_scale_in (fun f ->
             Aws.Query.Pair ("NewInstancesProtectedFromScaleIn", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TerminationPolicies.member"
              , TerminationPolicies.to_query v.termination_policies ))
       ; Some (Aws.Query.Pair ("Tags.member", TagDescriptionList.to_query v.tags))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("EnabledMetrics.member", EnabledMetrics.to_query v.enabled_metrics))
       ; Aws.Util.option_map v.v_p_c_zone_identifier (fun f ->
             Aws.Query.Pair ("VPCZoneIdentifier", String.to_query f))
       ; Aws.Util.option_map v.placement_group (fun f ->
             Aws.Query.Pair ("PlacementGroup", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SuspendedProcesses.member"
              , SuspendedProcesses.to_query v.suspended_processes ))
       ; Some (Aws.Query.Pair ("CreatedTime", DateTime.to_query v.created_time))
       ; Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances))
       ; Aws.Util.option_map v.health_check_grace_period (fun f ->
             Aws.Query.Pair ("HealthCheckGracePeriod", Integer.to_query f))
       ; Some (Aws.Query.Pair ("HealthCheckType", String.to_query v.health_check_type))
       ; Some
           (Aws.Query.Pair
              ("TargetGroupARNs.member", TargetGroupARNs.to_query v.target_group_a_r_ns))
       ; Some
           (Aws.Query.Pair
              ( "LoadBalancerNames.member"
              , LoadBalancerNames.to_query v.load_balancer_names ))
       ; Some
           (Aws.Query.Pair
              ( "AvailabilityZoneIds.member"
              , AvailabilityZoneIds.to_query v.availability_zone_ids ))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZones.member", AvailabilityZones.to_query v.availability_zones))
       ; Some (Aws.Query.Pair ("DefaultCooldown", Integer.to_query v.default_cooldown))
       ; Aws.Util.option_map v.predicted_capacity (fun f ->
             Aws.Query.Pair ("PredictedCapacity", Integer.to_query f))
       ; Some (Aws.Query.Pair ("DesiredCapacity", Integer.to_query v.desired_capacity))
       ; Some (Aws.Query.Pair ("MaxSize", Integer.to_query v.max_size))
       ; Some (Aws.Query.Pair ("MinSize", Integer.to_query v.min_size))
       ; Aws.Util.option_map v.mixed_instances_policy (fun f ->
             Aws.Query.Pair ("MixedInstancesPolicy", MixedInstancesPolicy.to_query f))
       ; Aws.Util.option_map v.launch_template (fun f ->
             Aws.Query.Pair ("LaunchTemplate", LaunchTemplateSpecification.to_query f))
       ; Aws.Util.option_map v.launch_configuration_name (fun f ->
             Aws.Query.Pair ("LaunchConfigurationName", String.to_query f))
       ; Aws.Util.option_map v.auto_scaling_group_a_r_n (fun f ->
             Aws.Query.Pair ("AutoScalingGroupARN", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_lifecycle_policy (fun f ->
             "InstanceLifecyclePolicy", InstanceLifecyclePolicy.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_specification (fun f ->
             ( "CapacityReservationSpecification"
             , CapacityReservationSpecification.to_json f ))
       ; Aws.Util.option_map v.availability_zone_impairment_policy (fun f ->
             ( "AvailabilityZoneImpairmentPolicy"
             , AvailabilityZoneImpairmentPolicy.to_json f ))
       ; Aws.Util.option_map v.availability_zone_distribution (fun f ->
             "AvailabilityZoneDistribution", AvailabilityZoneDistribution.to_json f)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", DeletionProtection.to_json f)
       ; Aws.Util.option_map v.instance_maintenance_policy (fun f ->
             "InstanceMaintenancePolicy", InstanceMaintenancePolicy.to_json f)
       ; Some ("TrafficSources", TrafficSources.to_json v.traffic_sources)
       ; Aws.Util.option_map v.default_instance_warmup (fun f ->
             "DefaultInstanceWarmup", Integer.to_json f)
       ; Aws.Util.option_map v.desired_capacity_type (fun f ->
             "DesiredCapacityType", String.to_json f)
       ; Aws.Util.option_map v.context (fun f -> "Context", String.to_json f)
       ; Aws.Util.option_map v.warm_pool_size (fun f -> "WarmPoolSize", Integer.to_json f)
       ; Aws.Util.option_map v.warm_pool_configuration (fun f ->
             "WarmPoolConfiguration", WarmPoolConfiguration.to_json f)
       ; Aws.Util.option_map v.capacity_rebalance (fun f ->
             "CapacityRebalance", Boolean.to_json f)
       ; Aws.Util.option_map v.max_instance_lifetime (fun f ->
             "MaxInstanceLifetime", Integer.to_json f)
       ; Aws.Util.option_map v.service_linked_role_a_r_n (fun f ->
             "ServiceLinkedRoleARN", String.to_json f)
       ; Aws.Util.option_map v.new_instances_protected_from_scale_in (fun f ->
             "NewInstancesProtectedFromScaleIn", Boolean.to_json f)
       ; Some ("TerminationPolicies", TerminationPolicies.to_json v.termination_policies)
       ; Some ("Tags", TagDescriptionList.to_json v.tags)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Some ("EnabledMetrics", EnabledMetrics.to_json v.enabled_metrics)
       ; Aws.Util.option_map v.v_p_c_zone_identifier (fun f ->
             "VPCZoneIdentifier", String.to_json f)
       ; Aws.Util.option_map v.placement_group (fun f ->
             "PlacementGroup", String.to_json f)
       ; Some ("SuspendedProcesses", SuspendedProcesses.to_json v.suspended_processes)
       ; Some ("CreatedTime", DateTime.to_json v.created_time)
       ; Some ("Instances", Instances.to_json v.instances)
       ; Aws.Util.option_map v.health_check_grace_period (fun f ->
             "HealthCheckGracePeriod", Integer.to_json f)
       ; Some ("HealthCheckType", String.to_json v.health_check_type)
       ; Some ("TargetGroupARNs", TargetGroupARNs.to_json v.target_group_a_r_ns)
       ; Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
       ; Some ("AvailabilityZoneIds", AvailabilityZoneIds.to_json v.availability_zone_ids)
       ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
       ; Some ("DefaultCooldown", Integer.to_json v.default_cooldown)
       ; Aws.Util.option_map v.predicted_capacity (fun f ->
             "PredictedCapacity", Integer.to_json f)
       ; Some ("DesiredCapacity", Integer.to_json v.desired_capacity)
       ; Some ("MaxSize", Integer.to_json v.max_size)
       ; Some ("MinSize", Integer.to_json v.min_size)
       ; Aws.Util.option_map v.mixed_instances_policy (fun f ->
             "MixedInstancesPolicy", MixedInstancesPolicy.to_json f)
       ; Aws.Util.option_map v.launch_template (fun f ->
             "LaunchTemplate", LaunchTemplateSpecification.to_json f)
       ; Aws.Util.option_map v.launch_configuration_name (fun f ->
             "LaunchConfigurationName", String.to_json f)
       ; Aws.Util.option_map v.auto_scaling_group_a_r_n (fun f ->
             "AutoScalingGroupARN", String.to_json f)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; auto_scaling_group_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupARN") String.of_json
  ; launch_configuration_name =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchConfigurationName") String.of_json
  ; launch_template =
      Aws.Util.option_map
        (Aws.Json.lookup j "LaunchTemplate")
        LaunchTemplateSpecification.of_json
  ; mixed_instances_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "MixedInstancesPolicy")
        MixedInstancesPolicy.of_json
  ; min_size = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MinSize"))
  ; max_size = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxSize"))
  ; desired_capacity =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DesiredCapacity"))
  ; predicted_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "PredictedCapacity") Integer.of_json
  ; default_cooldown =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DefaultCooldown"))
  ; availability_zones =
      AvailabilityZones.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
  ; availability_zone_ids =
      AvailabilityZoneIds.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZoneIds"))
  ; load_balancer_names =
      LoadBalancerNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
  ; target_group_a_r_ns =
      TargetGroupARNs.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetGroupARNs"))
  ; health_check_type =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckType"))
  ; health_check_grace_period =
      Aws.Util.option_map (Aws.Json.lookup j "HealthCheckGracePeriod") Integer.of_json
  ; instances = Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
  ; created_time =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CreatedTime"))
  ; suspended_processes =
      SuspendedProcesses.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SuspendedProcesses"))
  ; placement_group =
      Aws.Util.option_map (Aws.Json.lookup j "PlacementGroup") String.of_json
  ; v_p_c_zone_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "VPCZoneIdentifier") String.of_json
  ; enabled_metrics =
      EnabledMetrics.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EnabledMetrics"))
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; tags = TagDescriptionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; termination_policies =
      TerminationPolicies.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TerminationPolicies"))
  ; new_instances_protected_from_scale_in =
      Aws.Util.option_map
        (Aws.Json.lookup j "NewInstancesProtectedFromScaleIn")
        Boolean.of_json
  ; service_linked_role_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceLinkedRoleARN") String.of_json
  ; max_instance_lifetime =
      Aws.Util.option_map (Aws.Json.lookup j "MaxInstanceLifetime") Integer.of_json
  ; capacity_rebalance =
      Aws.Util.option_map (Aws.Json.lookup j "CapacityRebalance") Boolean.of_json
  ; warm_pool_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "WarmPoolConfiguration")
        WarmPoolConfiguration.of_json
  ; warm_pool_size =
      Aws.Util.option_map (Aws.Json.lookup j "WarmPoolSize") Integer.of_json
  ; context = Aws.Util.option_map (Aws.Json.lookup j "Context") String.of_json
  ; desired_capacity_type =
      Aws.Util.option_map (Aws.Json.lookup j "DesiredCapacityType") String.of_json
  ; default_instance_warmup =
      Aws.Util.option_map (Aws.Json.lookup j "DefaultInstanceWarmup") Integer.of_json
  ; traffic_sources =
      TrafficSources.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficSources"))
  ; instance_maintenance_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceMaintenancePolicy")
        InstanceMaintenancePolicy.of_json
  ; deletion_protection =
      Aws.Util.option_map
        (Aws.Json.lookup j "DeletionProtection")
        DeletionProtection.of_json
  ; availability_zone_distribution =
      Aws.Util.option_map
        (Aws.Json.lookup j "AvailabilityZoneDistribution")
        AvailabilityZoneDistribution.of_json
  ; availability_zone_impairment_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "AvailabilityZoneImpairmentPolicy")
        AvailabilityZoneImpairmentPolicy.of_json
  ; capacity_reservation_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "CapacityReservationSpecification")
        CapacityReservationSpecification.of_json
  ; instance_lifecycle_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceLifecyclePolicy")
        InstanceLifecyclePolicy.of_json
  }
