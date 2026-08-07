open Aws.BaseTypes

type t =
  { instance_types : InstanceTypes.t
  ; target_capacity : Integer.t
  ; target_capacity_unit_type : TargetCapacityUnitType.t option
  ; single_availability_zone : Boolean.t option
  ; region_names : RegionNames.t
  ; instance_requirements_with_metadata : InstanceRequirementsWithMetadataRequest.t option
  ; dry_run : Boolean.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make
    ?(instance_types = [])
    ~target_capacity
    ?target_capacity_unit_type
    ?single_availability_zone
    ?(region_names = [])
    ?instance_requirements_with_metadata
    ?dry_run
    ?max_results
    ?next_token
    () =
  { instance_types
  ; target_capacity
  ; target_capacity_unit_type
  ; single_availability_zone
  ; region_names
  ; instance_requirements_with_metadata
  ; dry_run
  ; max_results
  ; next_token
  }

let parse xml =
  Some
    { instance_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) InstanceTypes.parse)
    ; target_capacity =
        Aws.Xml.required
          "TargetCapacity"
          (Aws.Util.option_bind (Aws.Xml.member "TargetCapacity" xml) Integer.parse)
    ; target_capacity_unit_type =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetCapacityUnitType" xml)
          TargetCapacityUnitType.parse
    ; single_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "SingleAvailabilityZone" xml) Boolean.parse
    ; region_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "RegionName" xml) RegionNames.parse)
    ; instance_requirements_with_metadata =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceRequirementsWithMetadata" xml)
          InstanceRequirementsWithMetadataRequest.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.instance_requirements_with_metadata (fun f ->
             Aws.Query.Pair
               ( "InstanceRequirementsWithMetadata"
               , InstanceRequirementsWithMetadataRequest.to_query f ))
       ; Some (Aws.Query.Pair ("RegionName", RegionNames.to_query v.region_names))
       ; Aws.Util.option_map v.single_availability_zone (fun f ->
             Aws.Query.Pair ("SingleAvailabilityZone", Boolean.to_query f))
       ; Aws.Util.option_map v.target_capacity_unit_type (fun f ->
             Aws.Query.Pair ("TargetCapacityUnitType", TargetCapacityUnitType.to_query f))
       ; Some (Aws.Query.Pair ("TargetCapacity", Integer.to_query v.target_capacity))
       ; Some (Aws.Query.Pair ("InstanceType", InstanceTypes.to_query v.instance_types))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.instance_requirements_with_metadata (fun f ->
             ( "InstanceRequirementsWithMetadata"
             , InstanceRequirementsWithMetadataRequest.to_json f ))
       ; Some ("RegionName", RegionNames.to_json v.region_names)
       ; Aws.Util.option_map v.single_availability_zone (fun f ->
             "SingleAvailabilityZone", Boolean.to_json f)
       ; Aws.Util.option_map v.target_capacity_unit_type (fun f ->
             "TargetCapacityUnitType", TargetCapacityUnitType.to_json f)
       ; Some ("TargetCapacity", Integer.to_json v.target_capacity)
       ; Some ("InstanceType", InstanceTypes.to_json v.instance_types)
       ])

let of_json j =
  { instance_types =
      InstanceTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceType"))
  ; target_capacity =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetCapacity"))
  ; target_capacity_unit_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetCapacityUnitType")
        TargetCapacityUnitType.of_json
  ; single_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "SingleAvailabilityZone") Boolean.of_json
  ; region_names =
      RegionNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RegionName"))
  ; instance_requirements_with_metadata =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceRequirementsWithMetadata")
        InstanceRequirementsWithMetadataRequest.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
