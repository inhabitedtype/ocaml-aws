open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; traffic_sources : TrafficSources.t
  ; skip_zonal_shift_validation : Boolean.t option
  }

let make ~auto_scaling_group_name ~traffic_sources ?skip_zonal_shift_validation () =
  { auto_scaling_group_name; traffic_sources; skip_zonal_shift_validation }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; traffic_sources =
        Aws.Xml.required
          "TrafficSources"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficSources" xml)
             TrafficSources.parse)
    ; skip_zonal_shift_validation =
        Aws.Util.option_bind (Aws.Xml.member "SkipZonalShiftValidation" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.skip_zonal_shift_validation (fun f ->
             Aws.Query.Pair ("SkipZonalShiftValidation", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TrafficSources.member", TrafficSources.to_query v.traffic_sources))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.skip_zonal_shift_validation (fun f ->
             "SkipZonalShiftValidation", Boolean.to_json f)
       ; Some ("TrafficSources", TrafficSources.to_json v.traffic_sources)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; traffic_sources =
      TrafficSources.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficSources"))
  ; skip_zonal_shift_validation =
      Aws.Util.option_map (Aws.Json.lookup j "SkipZonalShiftValidation") Boolean.of_json
  }
