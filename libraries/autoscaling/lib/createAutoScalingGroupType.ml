open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; launch_configuration_name : String.t option
  ; launch_template : LaunchTemplateSpecification.t option
  ; mixed_instances_policy : MixedInstancesPolicy.t option
  ; instance_id : String.t option
  ; min_size : Integer.t
  ; max_size : Integer.t
  ; desired_capacity : Integer.t option
  ; default_cooldown : Integer.t option
  ; availability_zones : AvailabilityZones.t
  ; availability_zone_ids : AvailabilityZoneIds.t
  ; load_balancer_names : LoadBalancerNames.t
  ; target_group_a_r_ns : TargetGroupARNs.t
  ; health_check_type : String.t option
  ; health_check_grace_period : Integer.t option
  ; placement_group : String.t option
  ; v_p_c_zone_identifier : String.t option
  ; termination_policies : TerminationPolicies.t
  ; new_instances_protected_from_scale_in : Boolean.t option
  ; capacity_rebalance : Boolean.t option
  ; lifecycle_hook_specification_list : LifecycleHookSpecifications.t
  ; deletion_protection : DeletionProtection.t option
  ; tags : Tags.t
  ; service_linked_role_a_r_n : String.t option
  ; max_instance_lifetime : Integer.t option
  ; context : String.t option
  ; desired_capacity_type : String.t option
  ; default_instance_warmup : Integer.t option
  ; traffic_sources : TrafficSources.t
  ; instance_maintenance_policy : InstanceMaintenancePolicy.t option
  ; availability_zone_distribution : AvailabilityZoneDistribution.t option
  ; availability_zone_impairment_policy : AvailabilityZoneImpairmentPolicy.t option
  ; skip_zonal_shift_validation : Boolean.t option
  ; capacity_reservation_specification : CapacityReservationSpecification.t option
  ; instance_lifecycle_policy : InstanceLifecyclePolicy.t option
  }

let make
    ~auto_scaling_group_name
    ?launch_configuration_name
    ?launch_template
    ?mixed_instances_policy
    ?instance_id
    ~min_size
    ~max_size
    ?desired_capacity
    ?default_cooldown
    ?(availability_zones = [])
    ?(availability_zone_ids = [])
    ?(load_balancer_names = [])
    ?(target_group_a_r_ns = [])
    ?health_check_type
    ?health_check_grace_period
    ?placement_group
    ?v_p_c_zone_identifier
    ?(termination_policies = [])
    ?new_instances_protected_from_scale_in
    ?capacity_rebalance
    ?(lifecycle_hook_specification_list = [])
    ?deletion_protection
    ?(tags = [])
    ?service_linked_role_a_r_n
    ?max_instance_lifetime
    ?context
    ?desired_capacity_type
    ?default_instance_warmup
    ?(traffic_sources = [])
    ?instance_maintenance_policy
    ?availability_zone_distribution
    ?availability_zone_impairment_policy
    ?skip_zonal_shift_validation
    ?capacity_reservation_specification
    ?instance_lifecycle_policy
    () =
  { auto_scaling_group_name
  ; launch_configuration_name
  ; launch_template
  ; mixed_instances_policy
  ; instance_id
  ; min_size
  ; max_size
  ; desired_capacity
  ; default_cooldown
  ; availability_zones
  ; availability_zone_ids
  ; load_balancer_names
  ; target_group_a_r_ns
  ; health_check_type
  ; health_check_grace_period
  ; placement_group
  ; v_p_c_zone_identifier
  ; termination_policies
  ; new_instances_protected_from_scale_in
  ; capacity_rebalance
  ; lifecycle_hook_specification_list
  ; deletion_protection
  ; tags
  ; service_linked_role_a_r_n
  ; max_instance_lifetime
  ; context
  ; desired_capacity_type
  ; default_instance_warmup
  ; traffic_sources
  ; instance_maintenance_policy
  ; availability_zone_distribution
  ; availability_zone_impairment_policy
  ; skip_zonal_shift_validation
  ; capacity_reservation_specification
  ; instance_lifecycle_policy
  }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
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
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
    ; min_size =
        Aws.Xml.required
          "MinSize"
          (Aws.Util.option_bind (Aws.Xml.member "MinSize" xml) Integer.parse)
    ; max_size =
        Aws.Xml.required
          "MaxSize"
          (Aws.Util.option_bind (Aws.Xml.member "MaxSize" xml) Integer.parse)
    ; desired_capacity =
        Aws.Util.option_bind (Aws.Xml.member "DesiredCapacity" xml) Integer.parse
    ; default_cooldown =
        Aws.Util.option_bind (Aws.Xml.member "DefaultCooldown" xml) Integer.parse
    ; availability_zones =
        Aws.Util.of_option
          []
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
        Aws.Util.option_bind (Aws.Xml.member "HealthCheckType" xml) String.parse
    ; health_check_grace_period =
        Aws.Util.option_bind (Aws.Xml.member "HealthCheckGracePeriod" xml) Integer.parse
    ; placement_group =
        Aws.Util.option_bind (Aws.Xml.member "PlacementGroup" xml) String.parse
    ; v_p_c_zone_identifier =
        Aws.Util.option_bind (Aws.Xml.member "VPCZoneIdentifier" xml) String.parse
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
    ; capacity_rebalance =
        Aws.Util.option_bind (Aws.Xml.member "CapacityRebalance" xml) Boolean.parse
    ; lifecycle_hook_specification_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LifecycleHookSpecificationList" xml)
             LifecycleHookSpecifications.parse)
    ; deletion_protection =
        Aws.Util.option_bind
          (Aws.Xml.member "DeletionProtection" xml)
          DeletionProtection.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
    ; service_linked_role_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "ServiceLinkedRoleARN" xml) String.parse
    ; max_instance_lifetime =
        Aws.Util.option_bind (Aws.Xml.member "MaxInstanceLifetime" xml) Integer.parse
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
    ; availability_zone_distribution =
        Aws.Util.option_bind
          (Aws.Xml.member "AvailabilityZoneDistribution" xml)
          AvailabilityZoneDistribution.parse
    ; availability_zone_impairment_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "AvailabilityZoneImpairmentPolicy" xml)
          AvailabilityZoneImpairmentPolicy.parse
    ; skip_zonal_shift_validation =
        Aws.Util.option_bind (Aws.Xml.member "SkipZonalShiftValidation" xml) Boolean.parse
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
       ; Aws.Util.option_map v.skip_zonal_shift_validation (fun f ->
             Aws.Query.Pair ("SkipZonalShiftValidation", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone_impairment_policy (fun f ->
             Aws.Query.Pair
               ( "AvailabilityZoneImpairmentPolicy"
               , AvailabilityZoneImpairmentPolicy.to_query f ))
       ; Aws.Util.option_map v.availability_zone_distribution (fun f ->
             Aws.Query.Pair
               ("AvailabilityZoneDistribution", AvailabilityZoneDistribution.to_query f))
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
       ; Aws.Util.option_map v.max_instance_lifetime (fun f ->
             Aws.Query.Pair ("MaxInstanceLifetime", Integer.to_query f))
       ; Aws.Util.option_map v.service_linked_role_a_r_n (fun f ->
             Aws.Query.Pair ("ServiceLinkedRoleARN", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", DeletionProtection.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LifecycleHookSpecificationList.member"
              , LifecycleHookSpecifications.to_query v.lifecycle_hook_specification_list
              ))
       ; Aws.Util.option_map v.capacity_rebalance (fun f ->
             Aws.Query.Pair ("CapacityRebalance", Boolean.to_query f))
       ; Aws.Util.option_map v.new_instances_protected_from_scale_in (fun f ->
             Aws.Query.Pair ("NewInstancesProtectedFromScaleIn", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TerminationPolicies.member"
              , TerminationPolicies.to_query v.termination_policies ))
       ; Aws.Util.option_map v.v_p_c_zone_identifier (fun f ->
             Aws.Query.Pair ("VPCZoneIdentifier", String.to_query f))
       ; Aws.Util.option_map v.placement_group (fun f ->
             Aws.Query.Pair ("PlacementGroup", String.to_query f))
       ; Aws.Util.option_map v.health_check_grace_period (fun f ->
             Aws.Query.Pair ("HealthCheckGracePeriod", Integer.to_query f))
       ; Aws.Util.option_map v.health_check_type (fun f ->
             Aws.Query.Pair ("HealthCheckType", String.to_query f))
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
       ; Aws.Util.option_map v.default_cooldown (fun f ->
             Aws.Query.Pair ("DefaultCooldown", Integer.to_query f))
       ; Aws.Util.option_map v.desired_capacity (fun f ->
             Aws.Query.Pair ("DesiredCapacity", Integer.to_query f))
       ; Some (Aws.Query.Pair ("MaxSize", Integer.to_query v.max_size))
       ; Some (Aws.Query.Pair ("MinSize", Integer.to_query v.min_size))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.mixed_instances_policy (fun f ->
             Aws.Query.Pair ("MixedInstancesPolicy", MixedInstancesPolicy.to_query f))
       ; Aws.Util.option_map v.launch_template (fun f ->
             Aws.Query.Pair ("LaunchTemplate", LaunchTemplateSpecification.to_query f))
       ; Aws.Util.option_map v.launch_configuration_name (fun f ->
             Aws.Query.Pair ("LaunchConfigurationName", String.to_query f))
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
       ; Aws.Util.option_map v.skip_zonal_shift_validation (fun f ->
             "SkipZonalShiftValidation", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone_impairment_policy (fun f ->
             ( "AvailabilityZoneImpairmentPolicy"
             , AvailabilityZoneImpairmentPolicy.to_json f ))
       ; Aws.Util.option_map v.availability_zone_distribution (fun f ->
             "AvailabilityZoneDistribution", AvailabilityZoneDistribution.to_json f)
       ; Aws.Util.option_map v.instance_maintenance_policy (fun f ->
             "InstanceMaintenancePolicy", InstanceMaintenancePolicy.to_json f)
       ; Some ("TrafficSources", TrafficSources.to_json v.traffic_sources)
       ; Aws.Util.option_map v.default_instance_warmup (fun f ->
             "DefaultInstanceWarmup", Integer.to_json f)
       ; Aws.Util.option_map v.desired_capacity_type (fun f ->
             "DesiredCapacityType", String.to_json f)
       ; Aws.Util.option_map v.context (fun f -> "Context", String.to_json f)
       ; Aws.Util.option_map v.max_instance_lifetime (fun f ->
             "MaxInstanceLifetime", Integer.to_json f)
       ; Aws.Util.option_map v.service_linked_role_a_r_n (fun f ->
             "ServiceLinkedRoleARN", String.to_json f)
       ; Some ("Tags", Tags.to_json v.tags)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", DeletionProtection.to_json f)
       ; Some
           ( "LifecycleHookSpecificationList"
           , LifecycleHookSpecifications.to_json v.lifecycle_hook_specification_list )
       ; Aws.Util.option_map v.capacity_rebalance (fun f ->
             "CapacityRebalance", Boolean.to_json f)
       ; Aws.Util.option_map v.new_instances_protected_from_scale_in (fun f ->
             "NewInstancesProtectedFromScaleIn", Boolean.to_json f)
       ; Some ("TerminationPolicies", TerminationPolicies.to_json v.termination_policies)
       ; Aws.Util.option_map v.v_p_c_zone_identifier (fun f ->
             "VPCZoneIdentifier", String.to_json f)
       ; Aws.Util.option_map v.placement_group (fun f ->
             "PlacementGroup", String.to_json f)
       ; Aws.Util.option_map v.health_check_grace_period (fun f ->
             "HealthCheckGracePeriod", Integer.to_json f)
       ; Aws.Util.option_map v.health_check_type (fun f ->
             "HealthCheckType", String.to_json f)
       ; Some ("TargetGroupARNs", TargetGroupARNs.to_json v.target_group_a_r_ns)
       ; Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
       ; Some ("AvailabilityZoneIds", AvailabilityZoneIds.to_json v.availability_zone_ids)
       ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
       ; Aws.Util.option_map v.default_cooldown (fun f ->
             "DefaultCooldown", Integer.to_json f)
       ; Aws.Util.option_map v.desired_capacity (fun f ->
             "DesiredCapacity", Integer.to_json f)
       ; Some ("MaxSize", Integer.to_json v.max_size)
       ; Some ("MinSize", Integer.to_json v.min_size)
       ; Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f)
       ; Aws.Util.option_map v.mixed_instances_policy (fun f ->
             "MixedInstancesPolicy", MixedInstancesPolicy.to_json f)
       ; Aws.Util.option_map v.launch_template (fun f ->
             "LaunchTemplate", LaunchTemplateSpecification.to_json f)
       ; Aws.Util.option_map v.launch_configuration_name (fun f ->
             "LaunchConfigurationName", String.to_json f)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
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
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json
  ; min_size = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MinSize"))
  ; max_size = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxSize"))
  ; desired_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "DesiredCapacity") Integer.of_json
  ; default_cooldown =
      Aws.Util.option_map (Aws.Json.lookup j "DefaultCooldown") Integer.of_json
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
      Aws.Util.option_map (Aws.Json.lookup j "HealthCheckType") String.of_json
  ; health_check_grace_period =
      Aws.Util.option_map (Aws.Json.lookup j "HealthCheckGracePeriod") Integer.of_json
  ; placement_group =
      Aws.Util.option_map (Aws.Json.lookup j "PlacementGroup") String.of_json
  ; v_p_c_zone_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "VPCZoneIdentifier") String.of_json
  ; termination_policies =
      TerminationPolicies.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TerminationPolicies"))
  ; new_instances_protected_from_scale_in =
      Aws.Util.option_map
        (Aws.Json.lookup j "NewInstancesProtectedFromScaleIn")
        Boolean.of_json
  ; capacity_rebalance =
      Aws.Util.option_map (Aws.Json.lookup j "CapacityRebalance") Boolean.of_json
  ; lifecycle_hook_specification_list =
      LifecycleHookSpecifications.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookSpecificationList"))
  ; deletion_protection =
      Aws.Util.option_map
        (Aws.Json.lookup j "DeletionProtection")
        DeletionProtection.of_json
  ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; service_linked_role_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceLinkedRoleARN") String.of_json
  ; max_instance_lifetime =
      Aws.Util.option_map (Aws.Json.lookup j "MaxInstanceLifetime") Integer.of_json
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
  ; availability_zone_distribution =
      Aws.Util.option_map
        (Aws.Json.lookup j "AvailabilityZoneDistribution")
        AvailabilityZoneDistribution.of_json
  ; availability_zone_impairment_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "AvailabilityZoneImpairmentPolicy")
        AvailabilityZoneImpairmentPolicy.of_json
  ; skip_zonal_shift_validation =
      Aws.Util.option_map (Aws.Json.lookup j "SkipZonalShiftValidation") Boolean.of_json
  ; capacity_reservation_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "CapacityReservationSpecification")
        CapacityReservationSpecification.of_json
  ; instance_lifecycle_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceLifecyclePolicy")
        InstanceLifecyclePolicy.of_json
  }
