open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; bandwidth_weighting : InstanceBandwidthWeighting.t
  ; dry_run : Boolean.t option
  }

let make ~instance_id ~bandwidth_weighting ?dry_run () =
  { instance_id; bandwidth_weighting; dry_run }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; bandwidth_weighting =
        Aws.Xml.required
          "BandwidthWeighting"
          (Aws.Util.option_bind
             (Aws.Xml.member "BandwidthWeighting" xml)
             InstanceBandwidthWeighting.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BandwidthWeighting"
              , InstanceBandwidthWeighting.to_query v.bandwidth_weighting ))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ("BandwidthWeighting", InstanceBandwidthWeighting.to_json v.bandwidth_weighting)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; bandwidth_weighting =
      InstanceBandwidthWeighting.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BandwidthWeighting"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
