open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; description : String.t option
  ; launch_specification : ImportInstanceLaunchSpecification.t option
  ; disk_images : DiskImageList.t
  ; platform : PlatformValues.t
  }

let make ?dry_run ?description ?launch_specification ?(disk_images = []) ~platform () =
  { dry_run; description; launch_specification; disk_images; platform }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; launch_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "launchSpecification" xml)
          ImportInstanceLaunchSpecification.parse
    ; disk_images =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "diskImage" xml) DiskImageList.parse)
    ; platform =
        Aws.Xml.required
          "platform"
          (Aws.Util.option_bind (Aws.Xml.member "platform" xml) PlatformValues.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Platform", PlatformValues.to_query v.platform))
       ; Some (Aws.Query.Pair ("DiskImage", DiskImageList.to_query v.disk_images))
       ; Aws.Util.option_map v.launch_specification (fun f ->
             Aws.Query.Pair
               ("LaunchSpecification", ImportInstanceLaunchSpecification.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("platform", PlatformValues.to_json v.platform)
       ; Some ("diskImage", DiskImageList.to_json v.disk_images)
       ; Aws.Util.option_map v.launch_specification (fun f ->
             "launchSpecification", ImportInstanceLaunchSpecification.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; launch_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "launchSpecification")
        ImportInstanceLaunchSpecification.of_json
  ; disk_images =
      DiskImageList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "diskImage"))
  ; platform =
      PlatformValues.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "platform"))
  }
