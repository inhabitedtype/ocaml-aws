open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; instance_type : String.t option
  ; availability_zone : String.t
  ; availability_zone_id : String.t option
  ; lifecycle_state : LifecycleState.t
  ; health_status : String.t
  ; launch_configuration_name : String.t option
  ; launch_template : LaunchTemplateSpecification.t option
  ; image_id : String.t option
  ; protected_from_scale_in : Boolean.t
  ; weighted_capacity : String.t option
  }

let make
    ~instance_id
    ?instance_type
    ~availability_zone
    ?availability_zone_id
    ~lifecycle_state
    ~health_status
    ?launch_configuration_name
    ?launch_template
    ?image_id
    ~protected_from_scale_in
    ?weighted_capacity
    () =
  { instance_id
  ; instance_type
  ; availability_zone
  ; availability_zone_id
  ; lifecycle_state
  ; health_status
  ; launch_configuration_name
  ; launch_template
  ; image_id
  ; protected_from_scale_in
  ; weighted_capacity
  }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) String.parse
    ; availability_zone =
        Aws.Xml.required
          "AvailabilityZone"
          (Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse)
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; lifecycle_state =
        Aws.Xml.required
          "LifecycleState"
          (Aws.Util.option_bind
             (Aws.Xml.member "LifecycleState" xml)
             LifecycleState.parse)
    ; health_status =
        Aws.Xml.required
          "HealthStatus"
          (Aws.Util.option_bind (Aws.Xml.member "HealthStatus" xml) String.parse)
    ; launch_configuration_name =
        Aws.Util.option_bind (Aws.Xml.member "LaunchConfigurationName" xml) String.parse
    ; launch_template =
        Aws.Util.option_bind
          (Aws.Xml.member "LaunchTemplate" xml)
          LaunchTemplateSpecification.parse
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse
    ; protected_from_scale_in =
        Aws.Xml.required
          "ProtectedFromScaleIn"
          (Aws.Util.option_bind (Aws.Xml.member "ProtectedFromScaleIn" xml) Boolean.parse)
    ; weighted_capacity =
        Aws.Util.option_bind (Aws.Xml.member "WeightedCapacity" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.weighted_capacity (fun f ->
             Aws.Query.Pair ("WeightedCapacity", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ProtectedFromScaleIn", Boolean.to_query v.protected_from_scale_in))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Aws.Util.option_map v.launch_template (fun f ->
             Aws.Query.Pair ("LaunchTemplate", LaunchTemplateSpecification.to_query f))
       ; Aws.Util.option_map v.launch_configuration_name (fun f ->
             Aws.Query.Pair ("LaunchConfigurationName", String.to_query f))
       ; Some (Aws.Query.Pair ("HealthStatus", String.to_query v.health_status))
       ; Some
           (Aws.Query.Pair ("LifecycleState", LifecycleState.to_query v.lifecycle_state))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Some (Aws.Query.Pair ("AvailabilityZone", String.to_query v.availability_zone))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.weighted_capacity (fun f ->
             "WeightedCapacity", String.to_json f)
       ; Some ("ProtectedFromScaleIn", Boolean.to_json v.protected_from_scale_in)
       ; Aws.Util.option_map v.image_id (fun f -> "ImageId", String.to_json f)
       ; Aws.Util.option_map v.launch_template (fun f ->
             "LaunchTemplate", LaunchTemplateSpecification.to_json f)
       ; Aws.Util.option_map v.launch_configuration_name (fun f ->
             "LaunchConfigurationName", String.to_json f)
       ; Some ("HealthStatus", String.to_json v.health_status)
       ; Some ("LifecycleState", LifecycleState.to_json v.lifecycle_state)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Some ("AvailabilityZone", String.to_json v.availability_zone)
       ; Aws.Util.option_map v.instance_type (fun f -> "InstanceType", String.to_json f)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "InstanceType") String.of_json
  ; availability_zone =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZone"))
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; lifecycle_state =
      LifecycleState.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleState"))
  ; health_status =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthStatus"))
  ; launch_configuration_name =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchConfigurationName") String.of_json
  ; launch_template =
      Aws.Util.option_map
        (Aws.Json.lookup j "LaunchTemplate")
        LaunchTemplateSpecification.of_json
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "ImageId") String.of_json
  ; protected_from_scale_in =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ProtectedFromScaleIn"))
  ; weighted_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "WeightedCapacity") String.of_json
  }
