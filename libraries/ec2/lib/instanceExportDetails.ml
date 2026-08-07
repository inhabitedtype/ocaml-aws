open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; target_environment : ExportEnvironment.t option
  }

let make ?instance_id ?target_environment () = { instance_id; target_environment }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; target_environment =
        Aws.Util.option_bind
          (Aws.Xml.member "targetEnvironment" xml)
          ExportEnvironment.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_environment (fun f ->
             Aws.Query.Pair ("TargetEnvironment", ExportEnvironment.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_environment (fun f ->
             "targetEnvironment", ExportEnvironment.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; target_environment =
      Aws.Util.option_map
        (Aws.Json.lookup j "targetEnvironment")
        ExportEnvironment.of_json
  }
