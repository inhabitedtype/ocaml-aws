open Aws.BaseTypes

type t =
  { image_id : String.t option
  ; resource_type : FastLaunchResourceType.t option
  ; snapshot_configuration : FastLaunchSnapshotConfigurationResponse.t option
  ; launch_template : FastLaunchLaunchTemplateSpecificationResponse.t option
  ; max_parallel_launches : Integer.t option
  ; owner_id : String.t option
  ; state : FastLaunchStateCode.t option
  ; state_transition_reason : String.t option
  ; state_transition_time : DateTime.t option
  }

let make
    ?image_id
    ?resource_type
    ?snapshot_configuration
    ?launch_template
    ?max_parallel_launches
    ?owner_id
    ?state
    ?state_transition_reason
    ?state_transition_time
    () =
  { image_id
  ; resource_type
  ; snapshot_configuration
  ; launch_template
  ; max_parallel_launches
  ; owner_id
  ; state
  ; state_transition_reason
  ; state_transition_time
  }

let parse xml =
  Some
    { image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceType" xml)
          FastLaunchResourceType.parse
    ; snapshot_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "snapshotConfiguration" xml)
          FastLaunchSnapshotConfigurationResponse.parse
    ; launch_template =
        Aws.Util.option_bind
          (Aws.Xml.member "launchTemplate" xml)
          FastLaunchLaunchTemplateSpecificationResponse.parse
    ; max_parallel_launches =
        Aws.Util.option_bind (Aws.Xml.member "maxParallelLaunches" xml) Integer.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) FastLaunchStateCode.parse
    ; state_transition_reason =
        Aws.Util.option_bind (Aws.Xml.member "stateTransitionReason" xml) String.parse
    ; state_transition_time =
        Aws.Util.option_bind (Aws.Xml.member "stateTransitionTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_transition_time (fun f ->
             Aws.Query.Pair ("StateTransitionTime", DateTime.to_query f))
       ; Aws.Util.option_map v.state_transition_reason (fun f ->
             Aws.Query.Pair ("StateTransitionReason", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", FastLaunchStateCode.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.max_parallel_launches (fun f ->
             Aws.Query.Pair ("MaxParallelLaunches", Integer.to_query f))
       ; Aws.Util.option_map v.launch_template (fun f ->
             Aws.Query.Pair
               ("LaunchTemplate", FastLaunchLaunchTemplateSpecificationResponse.to_query f))
       ; Aws.Util.option_map v.snapshot_configuration (fun f ->
             Aws.Query.Pair
               ( "SnapshotConfiguration"
               , FastLaunchSnapshotConfigurationResponse.to_query f ))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", FastLaunchResourceType.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_transition_time (fun f ->
             "stateTransitionTime", DateTime.to_json f)
       ; Aws.Util.option_map v.state_transition_reason (fun f ->
             "stateTransitionReason", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", FastLaunchStateCode.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.max_parallel_launches (fun f ->
             "maxParallelLaunches", Integer.to_json f)
       ; Aws.Util.option_map v.launch_template (fun f ->
             "launchTemplate", FastLaunchLaunchTemplateSpecificationResponse.to_json f)
       ; Aws.Util.option_map v.snapshot_configuration (fun f ->
             "snapshotConfiguration", FastLaunchSnapshotConfigurationResponse.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", FastLaunchResourceType.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ])

let of_json j =
  { image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; resource_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceType")
        FastLaunchResourceType.of_json
  ; snapshot_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "snapshotConfiguration")
        FastLaunchSnapshotConfigurationResponse.of_json
  ; launch_template =
      Aws.Util.option_map
        (Aws.Json.lookup j "launchTemplate")
        FastLaunchLaunchTemplateSpecificationResponse.of_json
  ; max_parallel_launches =
      Aws.Util.option_map (Aws.Json.lookup j "maxParallelLaunches") Integer.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") FastLaunchStateCode.of_json
  ; state_transition_reason =
      Aws.Util.option_map (Aws.Json.lookup j "stateTransitionReason") String.of_json
  ; state_transition_time =
      Aws.Util.option_map (Aws.Json.lookup j "stateTransitionTime") DateTime.of_json
  }
