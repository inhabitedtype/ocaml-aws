open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; vpc_endpoint_ids : VpcEndpointIdList.t
  }

let make ?dry_run ~vpc_endpoint_ids () = { dry_run; vpc_endpoint_ids }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; vpc_endpoint_ids =
        Aws.Xml.required
          "VpcEndpointId"
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcEndpointId" xml)
             VpcEndpointIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("VpcEndpointId", VpcEndpointIdList.to_query v.vpc_endpoint_ids))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("VpcEndpointId", VpcEndpointIdList.to_json v.vpc_endpoint_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; vpc_endpoint_ids =
      VpcEndpointIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcEndpointId"))
  }
