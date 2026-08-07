open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instance_id : String.t
  ; wake_up : Boolean.t option
  }

let make ?dry_run ~instance_id ?wake_up () = { dry_run; instance_id; wake_up }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; wake_up = Aws.Util.option_bind (Aws.Xml.member "WakeUp" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.wake_up (fun f ->
             Aws.Query.Pair ("WakeUp", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.wake_up (fun f -> "WakeUp", Boolean.to_json f)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; wake_up = Aws.Util.option_map (Aws.Json.lookup j "WakeUp") Boolean.of_json
  }
