open Aws.BaseTypes

type t =
  { launch_specification : RequestSpotLaunchSpecification.t option
  ; tag_specifications : TagSpecificationList.t
  ; instance_interruption_behavior : InstanceInterruptionBehavior.t option
  ; dry_run : Boolean.t option
  ; spot_price : String.t option
  ; client_token : String.t option
  ; instance_count : Integer.t option
  ; type_ : SpotInstanceType.t option
  ; valid_from : DateTime.t option
  ; valid_until : DateTime.t option
  ; launch_group : String.t option
  ; availability_zone_group : String.t option
  ; block_duration_minutes : Integer.t option
  }

let make
    ?launch_specification
    ?(tag_specifications = [])
    ?instance_interruption_behavior
    ?dry_run
    ?spot_price
    ?client_token
    ?instance_count
    ?type_
    ?valid_from
    ?valid_until
    ?launch_group
    ?availability_zone_group
    ?block_duration_minutes
    () =
  { launch_specification
  ; tag_specifications
  ; instance_interruption_behavior
  ; dry_run
  ; spot_price
  ; client_token
  ; instance_count
  ; type_
  ; valid_from
  ; valid_until
  ; launch_group
  ; availability_zone_group
  ; block_duration_minutes
  }

let parse xml =
  Some
    { launch_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "LaunchSpecification" xml)
          RequestSpotLaunchSpecification.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; instance_interruption_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceInterruptionBehavior" xml)
          InstanceInterruptionBehavior.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; spot_price = Aws.Util.option_bind (Aws.Xml.member "spotPrice" xml) String.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; instance_count =
        Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) SpotInstanceType.parse
    ; valid_from = Aws.Util.option_bind (Aws.Xml.member "validFrom" xml) DateTime.parse
    ; valid_until = Aws.Util.option_bind (Aws.Xml.member "validUntil" xml) DateTime.parse
    ; launch_group = Aws.Util.option_bind (Aws.Xml.member "launchGroup" xml) String.parse
    ; availability_zone_group =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneGroup" xml) String.parse
    ; block_duration_minutes =
        Aws.Util.option_bind (Aws.Xml.member "blockDurationMinutes" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.block_duration_minutes (fun f ->
             Aws.Query.Pair ("BlockDurationMinutes", Integer.to_query f))
       ; Aws.Util.option_map v.availability_zone_group (fun f ->
             Aws.Query.Pair ("AvailabilityZoneGroup", String.to_query f))
       ; Aws.Util.option_map v.launch_group (fun f ->
             Aws.Query.Pair ("LaunchGroup", String.to_query f))
       ; Aws.Util.option_map v.valid_until (fun f ->
             Aws.Query.Pair ("ValidUntil", DateTime.to_query f))
       ; Aws.Util.option_map v.valid_from (fun f ->
             Aws.Query.Pair ("ValidFrom", DateTime.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", SpotInstanceType.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.spot_price (fun f ->
             Aws.Query.Pair ("SpotPrice", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.instance_interruption_behavior (fun f ->
             Aws.Query.Pair
               ("InstanceInterruptionBehavior", InstanceInterruptionBehavior.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.launch_specification (fun f ->
             Aws.Query.Pair
               ("LaunchSpecification", RequestSpotLaunchSpecification.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.block_duration_minutes (fun f ->
             "blockDurationMinutes", Integer.to_json f)
       ; Aws.Util.option_map v.availability_zone_group (fun f ->
             "availabilityZoneGroup", String.to_json f)
       ; Aws.Util.option_map v.launch_group (fun f -> "launchGroup", String.to_json f)
       ; Aws.Util.option_map v.valid_until (fun f -> "validUntil", DateTime.to_json f)
       ; Aws.Util.option_map v.valid_from (fun f -> "validFrom", DateTime.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", SpotInstanceType.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "instanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.spot_price (fun f -> "spotPrice", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.instance_interruption_behavior (fun f ->
             "InstanceInterruptionBehavior", InstanceInterruptionBehavior.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.launch_specification (fun f ->
             "LaunchSpecification", RequestSpotLaunchSpecification.to_json f)
       ])

let of_json j =
  { launch_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "LaunchSpecification")
        RequestSpotLaunchSpecification.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; instance_interruption_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceInterruptionBehavior")
        InstanceInterruptionBehavior.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; spot_price = Aws.Util.option_map (Aws.Json.lookup j "spotPrice") String.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "instanceCount") Integer.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") SpotInstanceType.of_json
  ; valid_from = Aws.Util.option_map (Aws.Json.lookup j "validFrom") DateTime.of_json
  ; valid_until = Aws.Util.option_map (Aws.Json.lookup j "validUntil") DateTime.of_json
  ; launch_group = Aws.Util.option_map (Aws.Json.lookup j "launchGroup") String.of_json
  ; availability_zone_group =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneGroup") String.of_json
  ; block_duration_minutes =
      Aws.Util.option_map (Aws.Json.lookup j "blockDurationMinutes") Integer.of_json
  }
