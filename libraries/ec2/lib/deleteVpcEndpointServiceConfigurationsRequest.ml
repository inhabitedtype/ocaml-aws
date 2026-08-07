open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; service_ids : VpcEndpointServiceIdList.t
  }

let make ?dry_run ~service_ids () = { dry_run; service_ids }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; service_ids =
        Aws.Xml.required
          "ServiceId"
          (Aws.Util.option_bind
             (Aws.Xml.member "ServiceId" xml)
             VpcEndpointServiceIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ServiceId", VpcEndpointServiceIdList.to_query v.service_ids))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ServiceId", VpcEndpointServiceIdList.to_json v.service_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; service_ids =
      VpcEndpointServiceIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceId"))
  }
