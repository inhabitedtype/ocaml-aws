open Aws.BaseTypes

type t =
  { instance_type : InstanceType.t option
  ; max_price : String.t option
  ; subnet_id : String.t option
  ; availability_zone : String.t option
  ; weighted_capacity : Double.t option
  ; priority : Double.t option
  ; placement : PlacementResponse.t option
  ; instance_requirements : InstanceRequirements.t option
  ; image_id : String.t option
  ; block_device_mappings : BlockDeviceMappingResponseList.t
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
    ?instance_requirements
    ?image_id
    ?(block_device_mappings = [])
    ?availability_zone_id
    () =
  { instance_type
  ; max_price
  ; subnet_id
  ; availability_zone
  ; weighted_capacity
  ; priority
  ; placement
  ; instance_requirements
  ; image_id
  ; block_device_mappings
  ; availability_zone_id
  }

let parse xml =
  Some
    { instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; max_price = Aws.Util.option_bind (Aws.Xml.member "maxPrice" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; weighted_capacity =
        Aws.Util.option_bind (Aws.Xml.member "weightedCapacity" xml) Double.parse
    ; priority = Aws.Util.option_bind (Aws.Xml.member "priority" xml) Double.parse
    ; placement =
        Aws.Util.option_bind (Aws.Xml.member "placement" xml) PlacementResponse.parse
    ; instance_requirements =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceRequirements" xml)
          InstanceRequirements.parse
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "blockDeviceMappingSet" xml)
             BlockDeviceMappingResponseList.parse)
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMappingSet"
              , BlockDeviceMappingResponseList.to_query v.block_device_mappings ))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             Aws.Query.Pair ("InstanceRequirements", InstanceRequirements.to_query f))
       ; Aws.Util.option_map v.placement (fun f ->
             Aws.Query.Pair ("Placement", PlacementResponse.to_query f))
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
             "availabilityZoneId", String.to_json f)
       ; Some
           ( "blockDeviceMappingSet"
           , BlockDeviceMappingResponseList.to_json v.block_device_mappings )
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             "instanceRequirements", InstanceRequirements.to_json f)
       ; Aws.Util.option_map v.placement (fun f ->
             "placement", PlacementResponse.to_json f)
       ; Aws.Util.option_map v.priority (fun f -> "priority", Double.to_json f)
       ; Aws.Util.option_map v.weighted_capacity (fun f ->
             "weightedCapacity", Double.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.max_price (fun f -> "maxPrice", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ])

let of_json j =
  { instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; max_price = Aws.Util.option_map (Aws.Json.lookup j "maxPrice") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; weighted_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "weightedCapacity") Double.of_json
  ; priority = Aws.Util.option_map (Aws.Json.lookup j "priority") Double.of_json
  ; placement =
      Aws.Util.option_map (Aws.Json.lookup j "placement") PlacementResponse.of_json
  ; instance_requirements =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceRequirements")
        InstanceRequirements.of_json
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; block_device_mappings =
      BlockDeviceMappingResponseList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "blockDeviceMappingSet"))
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  }
