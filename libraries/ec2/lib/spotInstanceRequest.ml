open Aws.BaseTypes

type t =
  { actual_block_hourly_price : String.t option
  ; availability_zone_group : String.t option
  ; block_duration_minutes : Integer.t option
  ; create_time : DateTime.t option
  ; fault : SpotInstanceStateFault.t option
  ; instance_id : String.t option
  ; launch_group : String.t option
  ; launch_specification : LaunchSpecification.t option
  ; launched_availability_zone : String.t option
  ; launched_availability_zone_id : String.t option
  ; product_description : RIProductDescription.t option
  ; spot_instance_request_id : String.t option
  ; spot_price : String.t option
  ; state : SpotInstanceState.t option
  ; status : SpotInstanceStatus.t option
  ; tags : TagList.t
  ; type_ : SpotInstanceType.t option
  ; valid_from : DateTime.t option
  ; valid_until : DateTime.t option
  ; instance_interruption_behavior : InstanceInterruptionBehavior.t option
  }

let make
    ?actual_block_hourly_price
    ?availability_zone_group
    ?block_duration_minutes
    ?create_time
    ?fault
    ?instance_id
    ?launch_group
    ?launch_specification
    ?launched_availability_zone
    ?launched_availability_zone_id
    ?product_description
    ?spot_instance_request_id
    ?spot_price
    ?state
    ?status
    ?(tags = [])
    ?type_
    ?valid_from
    ?valid_until
    ?instance_interruption_behavior
    () =
  { actual_block_hourly_price
  ; availability_zone_group
  ; block_duration_minutes
  ; create_time
  ; fault
  ; instance_id
  ; launch_group
  ; launch_specification
  ; launched_availability_zone
  ; launched_availability_zone_id
  ; product_description
  ; spot_instance_request_id
  ; spot_price
  ; state
  ; status
  ; tags
  ; type_
  ; valid_from
  ; valid_until
  ; instance_interruption_behavior
  }

let parse xml =
  Some
    { actual_block_hourly_price =
        Aws.Util.option_bind (Aws.Xml.member "actualBlockHourlyPrice" xml) String.parse
    ; availability_zone_group =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneGroup" xml) String.parse
    ; block_duration_minutes =
        Aws.Util.option_bind (Aws.Xml.member "blockDurationMinutes" xml) Integer.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; fault =
        Aws.Util.option_bind (Aws.Xml.member "fault" xml) SpotInstanceStateFault.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; launch_group = Aws.Util.option_bind (Aws.Xml.member "launchGroup" xml) String.parse
    ; launch_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "launchSpecification" xml)
          LaunchSpecification.parse
    ; launched_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "launchedAvailabilityZone" xml) String.parse
    ; launched_availability_zone_id =
        Aws.Util.option_bind
          (Aws.Xml.member "launchedAvailabilityZoneId" xml)
          String.parse
    ; product_description =
        Aws.Util.option_bind
          (Aws.Xml.member "productDescription" xml)
          RIProductDescription.parse
    ; spot_instance_request_id =
        Aws.Util.option_bind (Aws.Xml.member "spotInstanceRequestId" xml) String.parse
    ; spot_price = Aws.Util.option_bind (Aws.Xml.member "spotPrice" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) SpotInstanceState.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) SpotInstanceStatus.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) SpotInstanceType.parse
    ; valid_from = Aws.Util.option_bind (Aws.Xml.member "validFrom" xml) DateTime.parse
    ; valid_until = Aws.Util.option_bind (Aws.Xml.member "validUntil" xml) DateTime.parse
    ; instance_interruption_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceInterruptionBehavior" xml)
          InstanceInterruptionBehavior.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_interruption_behavior (fun f ->
             Aws.Query.Pair
               ("InstanceInterruptionBehavior", InstanceInterruptionBehavior.to_query f))
       ; Aws.Util.option_map v.valid_until (fun f ->
             Aws.Query.Pair ("ValidUntil", DateTime.to_query f))
       ; Aws.Util.option_map v.valid_from (fun f ->
             Aws.Query.Pair ("ValidFrom", DateTime.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", SpotInstanceType.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", SpotInstanceStatus.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", SpotInstanceState.to_query f))
       ; Aws.Util.option_map v.spot_price (fun f ->
             Aws.Query.Pair ("SpotPrice", String.to_query f))
       ; Aws.Util.option_map v.spot_instance_request_id (fun f ->
             Aws.Query.Pair ("SpotInstanceRequestId", String.to_query f))
       ; Aws.Util.option_map v.product_description (fun f ->
             Aws.Query.Pair ("ProductDescription", RIProductDescription.to_query f))
       ; Aws.Util.option_map v.launched_availability_zone_id (fun f ->
             Aws.Query.Pair ("LaunchedAvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.launched_availability_zone (fun f ->
             Aws.Query.Pair ("LaunchedAvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.launch_specification (fun f ->
             Aws.Query.Pair ("LaunchSpecification", LaunchSpecification.to_query f))
       ; Aws.Util.option_map v.launch_group (fun f ->
             Aws.Query.Pair ("LaunchGroup", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.fault (fun f ->
             Aws.Query.Pair ("Fault", SpotInstanceStateFault.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.block_duration_minutes (fun f ->
             Aws.Query.Pair ("BlockDurationMinutes", Integer.to_query f))
       ; Aws.Util.option_map v.availability_zone_group (fun f ->
             Aws.Query.Pair ("AvailabilityZoneGroup", String.to_query f))
       ; Aws.Util.option_map v.actual_block_hourly_price (fun f ->
             Aws.Query.Pair ("ActualBlockHourlyPrice", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_interruption_behavior (fun f ->
             "instanceInterruptionBehavior", InstanceInterruptionBehavior.to_json f)
       ; Aws.Util.option_map v.valid_until (fun f -> "validUntil", DateTime.to_json f)
       ; Aws.Util.option_map v.valid_from (fun f -> "validFrom", DateTime.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", SpotInstanceType.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.status (fun f -> "status", SpotInstanceStatus.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", SpotInstanceState.to_json f)
       ; Aws.Util.option_map v.spot_price (fun f -> "spotPrice", String.to_json f)
       ; Aws.Util.option_map v.spot_instance_request_id (fun f ->
             "spotInstanceRequestId", String.to_json f)
       ; Aws.Util.option_map v.product_description (fun f ->
             "productDescription", RIProductDescription.to_json f)
       ; Aws.Util.option_map v.launched_availability_zone_id (fun f ->
             "launchedAvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.launched_availability_zone (fun f ->
             "launchedAvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.launch_specification (fun f ->
             "launchSpecification", LaunchSpecification.to_json f)
       ; Aws.Util.option_map v.launch_group (fun f -> "launchGroup", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.fault (fun f -> "fault", SpotInstanceStateFault.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ; Aws.Util.option_map v.block_duration_minutes (fun f ->
             "blockDurationMinutes", Integer.to_json f)
       ; Aws.Util.option_map v.availability_zone_group (fun f ->
             "availabilityZoneGroup", String.to_json f)
       ; Aws.Util.option_map v.actual_block_hourly_price (fun f ->
             "actualBlockHourlyPrice", String.to_json f)
       ])

let of_json j =
  { actual_block_hourly_price =
      Aws.Util.option_map (Aws.Json.lookup j "actualBlockHourlyPrice") String.of_json
  ; availability_zone_group =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneGroup") String.of_json
  ; block_duration_minutes =
      Aws.Util.option_map (Aws.Json.lookup j "blockDurationMinutes") Integer.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; fault = Aws.Util.option_map (Aws.Json.lookup j "fault") SpotInstanceStateFault.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; launch_group = Aws.Util.option_map (Aws.Json.lookup j "launchGroup") String.of_json
  ; launch_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "launchSpecification")
        LaunchSpecification.of_json
  ; launched_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "launchedAvailabilityZone") String.of_json
  ; launched_availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "launchedAvailabilityZoneId") String.of_json
  ; product_description =
      Aws.Util.option_map
        (Aws.Json.lookup j "productDescription")
        RIProductDescription.of_json
  ; spot_instance_request_id =
      Aws.Util.option_map (Aws.Json.lookup j "spotInstanceRequestId") String.of_json
  ; spot_price = Aws.Util.option_map (Aws.Json.lookup j "spotPrice") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") SpotInstanceState.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") SpotInstanceStatus.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") SpotInstanceType.of_json
  ; valid_from = Aws.Util.option_map (Aws.Json.lookup j "validFrom") DateTime.of_json
  ; valid_until = Aws.Util.option_map (Aws.Json.lookup j "validUntil") DateTime.of_json
  ; instance_interruption_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceInterruptionBehavior")
        InstanceInterruptionBehavior.of_json
  }
