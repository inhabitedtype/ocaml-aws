open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instance_id : String.t
  ; attribute : InstanceAttributeName.t
  }

let make ?dry_run ~instance_id ~attribute () = { dry_run; instance_id; attribute }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; instance_id =
        Aws.Xml.required
          "instanceId"
          (Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse)
    ; attribute =
        Aws.Xml.required
          "attribute"
          (Aws.Util.option_bind
             (Aws.Xml.member "attribute" xml)
             InstanceAttributeName.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Attribute", InstanceAttributeName.to_query v.attribute))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("attribute", InstanceAttributeName.to_json v.attribute)
       ; Some ("instanceId", String.to_json v.instance_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceId"))
  ; attribute =
      InstanceAttributeName.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "attribute"))
  }
