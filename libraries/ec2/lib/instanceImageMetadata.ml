open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; instance_type : InstanceType.t option
  ; launch_time : DateTime.t option
  ; availability_zone : String.t option
  ; zone_id : String.t option
  ; state : InstanceState.t option
  ; owner_id : String.t option
  ; tags : TagList.t
  ; image_metadata : ImageMetadata.t option
  ; operator : OperatorResponse.t option
  }

let make
    ?instance_id
    ?instance_type
    ?launch_time
    ?availability_zone
    ?zone_id
    ?state
    ?owner_id
    ?(tags = [])
    ?image_metadata
    ?operator
    () =
  { instance_id
  ; instance_type
  ; launch_time
  ; availability_zone
  ; zone_id
  ; state
  ; owner_id
  ; tags
  ; image_metadata
  ; operator
  }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; launch_time = Aws.Util.option_bind (Aws.Xml.member "launchTime" xml) DateTime.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; zone_id = Aws.Util.option_bind (Aws.Xml.member "zoneId" xml) String.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "instanceState" xml) InstanceState.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "instanceOwnerId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; image_metadata =
        Aws.Util.option_bind (Aws.Xml.member "imageMetadata" xml) ImageMetadata.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.image_metadata (fun f ->
             Aws.Query.Pair ("ImageMetadata", ImageMetadata.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("InstanceOwnerId", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("InstanceState", InstanceState.to_query f))
       ; Aws.Util.option_map v.zone_id (fun f ->
             Aws.Query.Pair ("ZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.launch_time (fun f ->
             Aws.Query.Pair ("LaunchTime", DateTime.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.image_metadata (fun f ->
             "imageMetadata", ImageMetadata.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.owner_id (fun f -> "instanceOwnerId", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "instanceState", InstanceState.to_json f)
       ; Aws.Util.option_map v.zone_id (fun f -> "zoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.launch_time (fun f -> "launchTime", DateTime.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; launch_time = Aws.Util.option_map (Aws.Json.lookup j "launchTime") DateTime.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; zone_id = Aws.Util.option_map (Aws.Json.lookup j "zoneId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "instanceState") InstanceState.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "instanceOwnerId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; image_metadata =
      Aws.Util.option_map (Aws.Json.lookup j "imageMetadata") ImageMetadata.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  }
