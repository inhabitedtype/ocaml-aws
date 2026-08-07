open Aws.BaseTypes

type t =
  { instance_type : InstanceType.t option
  ; max_price : String.t option
  ; subnet_id : String.t option
  ; availability_zone : String.t option
  ; weighted_capacity : Double.t option
  ; priority : Double.t option
  ; placement : Placement.t option
  ; block_device_mappings : FleetBlockDeviceMappingRequestList.t
  ; instance_requirements : InstanceRequirementsRequest.t option
  ; image_id : String.t option
  ; availability_zone_id : String.t option
  }

let make
    ?instance_type
    ?max_price
    ?subnet_id
    ?availability_zone
    ?weighted_capacity
    ?priority
    ?placement
    ?(block_device_mappings = [])
    ?instance_requirements
    ?image_id
    ?availability_zone_id
    () =
  { instance_type
  ; max_price
  ; subnet_id
  ; availability_zone
  ; weighted_capacity
  ; priority
  ; placement
  ; block_device_mappings
  ; instance_requirements
  ; image_id
  ; availability_zone_id
  }

let parse xml =
  Some
    { instance_type =
        Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) InstanceType.parse
    ; max_price = Aws.Util.option_bind (Aws.Xml.member "MaxPrice" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; weighted_capacity =
        Aws.Util.option_bind (Aws.Xml.member "WeightedCapacity" xml) Double.parse
    ; priority = Aws.Util.option_bind (Aws.Xml.member "Priority" xml) Double.parse
    ; placement = Aws.Util.option_bind (Aws.Xml.member "Placement" xml) Placement.parse
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "BlockDeviceMapping" xml)
             FleetBlockDeviceMappingRequestList.parse)
    ; instance_requirements =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceRequirements" xml)
          InstanceRequirementsRequest.parse
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             Aws.Query.Pair
               ("InstanceRequirements", InstanceRequirementsRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , FleetBlockDeviceMappingRequestList.to_query v.block_device_mappings ))
       ; Aws.Util.option_map v.placement (fun f ->
             Aws.Query.Pair ("Placement", Placement.to_query f))
       ; Aws.Util.option_map v.priority (fun f ->
             Aws.Query.Pair ("Priority", Double.to_query f))
       ; Aws.Util.option_map v.weighted_capacity (fun f ->
             Aws.Query.Pair ("WeightedCapacity", Double.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.max_price (fun f ->
             Aws.Query.Pair ("MaxPrice", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "ImageId", String.to_json f)
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             "InstanceRequirements", InstanceRequirementsRequest.to_json f)
       ; Some
           ( "BlockDeviceMapping"
           , FleetBlockDeviceMappingRequestList.to_json v.block_device_mappings )
       ; Aws.Util.option_map v.placement (fun f -> "Placement", Placement.to_json f)
       ; Aws.Util.option_map v.priority (fun f -> "Priority", Double.to_json f)
       ; Aws.Util.option_map v.weighted_capacity (fun f ->
             "WeightedCapacity", Double.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "SubnetId", String.to_json f)
       ; Aws.Util.option_map v.max_price (fun f -> "MaxPrice", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "InstanceType", InstanceType.to_json f)
       ])

let of_json j =
  { instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceType") InstanceType.of_json
  ; max_price = Aws.Util.option_map (Aws.Json.lookup j "MaxPrice") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "SubnetId") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; weighted_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "WeightedCapacity") Double.of_json
  ; priority = Aws.Util.option_map (Aws.Json.lookup j "Priority") Double.of_json
  ; placement = Aws.Util.option_map (Aws.Json.lookup j "Placement") Placement.of_json
  ; block_device_mappings =
      FleetBlockDeviceMappingRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BlockDeviceMapping"))
  ; instance_requirements =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceRequirements")
        InstanceRequirementsRequest.of_json
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "ImageId") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  }
