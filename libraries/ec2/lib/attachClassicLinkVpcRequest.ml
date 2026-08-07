open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instance_id : String.t
  ; vpc_id : String.t
  ; groups : GroupIdStringList.t
  }

let make ?dry_run ~instance_id ~vpc_id ~groups () =
  { dry_run; instance_id; vpc_id; groups }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; instance_id =
        Aws.Xml.required
          "instanceId"
          (Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse)
    ; vpc_id =
        Aws.Xml.required
          "vpcId"
          (Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse)
    ; groups =
        Aws.Xml.required
          "SecurityGroupId"
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             GroupIdStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("SecurityGroupId", GroupIdStringList.to_query v.groups))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SecurityGroupId", GroupIdStringList.to_json v.groups)
       ; Some ("vpcId", String.to_json v.vpc_id)
       ; Some ("instanceId", String.to_json v.instance_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceId"))
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcId"))
  ; groups =
      GroupIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  }
