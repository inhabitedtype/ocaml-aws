open Aws.BaseTypes

type t =
  { description : String.t option
  ; instance_id : String.t option
  ; platform : PlatformValues.t option
  ; volumes : ImportInstanceVolumeDetailSet.t
  }

let make ?description ?instance_id ?platform ?(volumes = []) () =
  { description; instance_id; platform; volumes }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; platform = Aws.Util.option_bind (Aws.Xml.member "platform" xml) PlatformValues.parse
    ; volumes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "volumes" xml)
             ImportInstanceVolumeDetailSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Volumes", ImportInstanceVolumeDetailSet.to_query v.volumes))
       ; Aws.Util.option_map v.platform (fun f ->
             Aws.Query.Pair ("Platform", PlatformValues.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("volumes", ImportInstanceVolumeDetailSet.to_json v.volumes)
       ; Aws.Util.option_map v.platform (fun f -> "platform", PlatformValues.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; platform = Aws.Util.option_map (Aws.Json.lookup j "platform") PlatformValues.of_json
  ; volumes =
      ImportInstanceVolumeDetailSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "volumes"))
  }
