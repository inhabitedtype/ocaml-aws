open Aws.BaseTypes

type t =
  { replace_root_volume_tasks : ReplaceRootVolumeTasks.t
  ; next_token : String.t option
  }

let make ?(replace_root_volume_tasks = []) ?next_token () =
  { replace_root_volume_tasks; next_token }

let parse xml =
  Some
    { replace_root_volume_tasks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "replaceRootVolumeTaskSet" xml)
             ReplaceRootVolumeTasks.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReplaceRootVolumeTaskSet"
              , ReplaceRootVolumeTasks.to_query v.replace_root_volume_tasks ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "replaceRootVolumeTaskSet"
           , ReplaceRootVolumeTasks.to_json v.replace_root_volume_tasks )
       ])

let of_json j =
  { replace_root_volume_tasks =
      ReplaceRootVolumeTasks.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "replaceRootVolumeTaskSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
