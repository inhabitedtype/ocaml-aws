open Aws.BaseTypes

type t =
  { vpc_id : String.t
  ; instance_tenancy : VpcTenancy.t
  ; dry_run : Boolean.t option
  }

let make ~vpc_id ~instance_tenancy ?dry_run () = { vpc_id; instance_tenancy; dry_run }

let parse xml =
  Some
    { vpc_id =
        Aws.Xml.required
          "VpcId"
          (Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse)
    ; instance_tenancy =
        Aws.Xml.required
          "InstanceTenancy"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceTenancy" xml) VpcTenancy.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("InstanceTenancy", VpcTenancy.to_query v.instance_tenancy))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("InstanceTenancy", VpcTenancy.to_json v.instance_tenancy)
       ; Some ("VpcId", String.to_json v.vpc_id)
       ])

let of_json j =
  { vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcId"))
  ; instance_tenancy =
      VpcTenancy.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceTenancy"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
