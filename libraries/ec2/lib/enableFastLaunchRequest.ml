open Aws.BaseTypes

type t =
  { image_id : String.t
  ; resource_type : String.t option
  ; snapshot_configuration : FastLaunchSnapshotConfigurationRequest.t option
  ; launch_template : FastLaunchLaunchTemplateSpecificationRequest.t option
  ; max_parallel_launches : Integer.t option
  ; dry_run : Boolean.t option
  }

let make
    ~image_id
    ?resource_type
    ?snapshot_configuration
    ?launch_template
    ?max_parallel_launches
    ?dry_run
    () =
  { image_id
  ; resource_type
  ; snapshot_configuration
  ; launch_template
  ; max_parallel_launches
  ; dry_run
  }

let parse xml =
  Some
    { image_id =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse)
    ; resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
    ; snapshot_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "SnapshotConfiguration" xml)
          FastLaunchSnapshotConfigurationRequest.parse
    ; launch_template =
        Aws.Util.option_bind
          (Aws.Xml.member "LaunchTemplate" xml)
          FastLaunchLaunchTemplateSpecificationRequest.parse
    ; max_parallel_launches =
        Aws.Util.option_bind (Aws.Xml.member "MaxParallelLaunches" xml) Integer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.max_parallel_launches (fun f ->
             Aws.Query.Pair ("MaxParallelLaunches", Integer.to_query f))
       ; Aws.Util.option_map v.launch_template (fun f ->
             Aws.Query.Pair
               ("LaunchTemplate", FastLaunchLaunchTemplateSpecificationRequest.to_query f))
       ; Aws.Util.option_map v.snapshot_configuration (fun f ->
             Aws.Query.Pair
               ("SnapshotConfiguration", FastLaunchSnapshotConfigurationRequest.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.max_parallel_launches (fun f ->
             "MaxParallelLaunches", Integer.to_json f)
       ; Aws.Util.option_map v.launch_template (fun f ->
             "LaunchTemplate", FastLaunchLaunchTemplateSpecificationRequest.to_json f)
       ; Aws.Util.option_map v.snapshot_configuration (fun f ->
             "SnapshotConfiguration", FastLaunchSnapshotConfigurationRequest.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
       ; Some ("ImageId", String.to_json v.image_id)
       ])

let of_json j =
  { image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; resource_type = Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
  ; snapshot_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "SnapshotConfiguration")
        FastLaunchSnapshotConfigurationRequest.of_json
  ; launch_template =
      Aws.Util.option_map
        (Aws.Json.lookup j "LaunchTemplate")
        FastLaunchLaunchTemplateSpecificationRequest.of_json
  ; max_parallel_launches =
      Aws.Util.option_map (Aws.Json.lookup j "MaxParallelLaunches") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
