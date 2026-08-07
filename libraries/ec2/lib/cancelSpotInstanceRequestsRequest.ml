open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; spot_instance_request_ids : SpotInstanceRequestIdList.t
  }

let make ?dry_run ~spot_instance_request_ids () = { dry_run; spot_instance_request_ids }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; spot_instance_request_ids =
        Aws.Xml.required
          "SpotInstanceRequestId"
          (Aws.Util.option_bind
             (Aws.Xml.member "SpotInstanceRequestId" xml)
             SpotInstanceRequestIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SpotInstanceRequestId"
              , SpotInstanceRequestIdList.to_query v.spot_instance_request_ids ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "SpotInstanceRequestId"
           , SpotInstanceRequestIdList.to_json v.spot_instance_request_ids )
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; spot_instance_request_ids =
      SpotInstanceRequestIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SpotInstanceRequestId"))
  }
