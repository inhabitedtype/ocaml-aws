open Aws.BaseTypes

type t =
  { zonal_shift_enabled : Boolean.t option
  ; impaired_zone_health_check_behavior : ImpairedZoneHealthCheckBehavior.t option
  }

let make ?zonal_shift_enabled ?impaired_zone_health_check_behavior () =
  { zonal_shift_enabled; impaired_zone_health_check_behavior }

let parse xml =
  Some
    { zonal_shift_enabled =
        Aws.Util.option_bind (Aws.Xml.member "ZonalShiftEnabled" xml) Boolean.parse
    ; impaired_zone_health_check_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "ImpairedZoneHealthCheckBehavior" xml)
          ImpairedZoneHealthCheckBehavior.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.impaired_zone_health_check_behavior (fun f ->
             Aws.Query.Pair
               ( "ImpairedZoneHealthCheckBehavior"
               , ImpairedZoneHealthCheckBehavior.to_query f ))
       ; Aws.Util.option_map v.zonal_shift_enabled (fun f ->
             Aws.Query.Pair ("ZonalShiftEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.impaired_zone_health_check_behavior (fun f ->
             "ImpairedZoneHealthCheckBehavior", ImpairedZoneHealthCheckBehavior.to_json f)
       ; Aws.Util.option_map v.zonal_shift_enabled (fun f ->
             "ZonalShiftEnabled", Boolean.to_json f)
       ])

let of_json j =
  { zonal_shift_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "ZonalShiftEnabled") Boolean.of_json
  ; impaired_zone_health_check_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "ImpairedZoneHealthCheckBehavior")
        ImpairedZoneHealthCheckBehavior.of_json
  }
