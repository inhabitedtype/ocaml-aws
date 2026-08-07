open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; vpc_peering_connection_id : String.t
  }

let make ?dry_run ~vpc_peering_connection_id () = { dry_run; vpc_peering_connection_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; vpc_peering_connection_id =
        Aws.Xml.required
          "vpcPeeringConnectionId"
          (Aws.Util.option_bind
             (Aws.Xml.member "vpcPeeringConnectionId" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("VpcPeeringConnectionId", String.to_query v.vpc_peering_connection_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("vpcPeeringConnectionId", String.to_json v.vpc_peering_connection_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; vpc_peering_connection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcPeeringConnectionId"))
  }
