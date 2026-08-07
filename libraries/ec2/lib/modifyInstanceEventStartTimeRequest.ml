open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instance_id : String.t
  ; instance_event_id : String.t
  ; not_before : DateTime.t
  }

let make ?dry_run ~instance_id ~instance_event_id ~not_before () =
  { dry_run; instance_id; instance_event_id; not_before }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; instance_event_id =
        Aws.Xml.required
          "InstanceEventId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceEventId" xml) String.parse)
    ; not_before =
        Aws.Xml.required
          "NotBefore"
          (Aws.Util.option_bind (Aws.Xml.member "NotBefore" xml) DateTime.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("NotBefore", DateTime.to_query v.not_before))
       ; Some (Aws.Query.Pair ("InstanceEventId", String.to_query v.instance_event_id))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("NotBefore", DateTime.to_json v.not_before)
       ; Some ("InstanceEventId", String.to_json v.instance_event_id)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; instance_event_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceEventId"))
  ; not_before = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NotBefore"))
  }
