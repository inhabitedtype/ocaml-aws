type t =
  { cross_zone_load_balancing : CrossZoneLoadBalancing.t option
  ; access_log : AccessLog.t option
  ; connection_draining : ConnectionDraining.t option
  ; connection_settings : ConnectionSettings.t option
  ; additional_attributes : AdditionalAttributes.t
  }

let make
    ?cross_zone_load_balancing
    ?access_log
    ?connection_draining
    ?connection_settings
    ?(additional_attributes = [])
    () =
  { cross_zone_load_balancing
  ; access_log
  ; connection_draining
  ; connection_settings
  ; additional_attributes
  }

let parse xml =
  Some
    { cross_zone_load_balancing =
        Aws.Util.option_bind
          (Aws.Xml.member "CrossZoneLoadBalancing" xml)
          CrossZoneLoadBalancing.parse
    ; access_log = Aws.Util.option_bind (Aws.Xml.member "AccessLog" xml) AccessLog.parse
    ; connection_draining =
        Aws.Util.option_bind
          (Aws.Xml.member "ConnectionDraining" xml)
          ConnectionDraining.parse
    ; connection_settings =
        Aws.Util.option_bind
          (Aws.Xml.member "ConnectionSettings" xml)
          ConnectionSettings.parse
    ; additional_attributes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AdditionalAttributes" xml)
             AdditionalAttributes.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AdditionalAttributes.member"
              , AdditionalAttributes.to_query v.additional_attributes ))
       ; Aws.Util.option_map v.connection_settings (fun f ->
             Aws.Query.Pair ("ConnectionSettings", ConnectionSettings.to_query f))
       ; Aws.Util.option_map v.connection_draining (fun f ->
             Aws.Query.Pair ("ConnectionDraining", ConnectionDraining.to_query f))
       ; Aws.Util.option_map v.access_log (fun f ->
             Aws.Query.Pair ("AccessLog", AccessLog.to_query f))
       ; Aws.Util.option_map v.cross_zone_load_balancing (fun f ->
             Aws.Query.Pair ("CrossZoneLoadBalancing", CrossZoneLoadBalancing.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("AdditionalAttributes", AdditionalAttributes.to_json v.additional_attributes)
       ; Aws.Util.option_map v.connection_settings (fun f ->
             "ConnectionSettings", ConnectionSettings.to_json f)
       ; Aws.Util.option_map v.connection_draining (fun f ->
             "ConnectionDraining", ConnectionDraining.to_json f)
       ; Aws.Util.option_map v.access_log (fun f -> "AccessLog", AccessLog.to_json f)
       ; Aws.Util.option_map v.cross_zone_load_balancing (fun f ->
             "CrossZoneLoadBalancing", CrossZoneLoadBalancing.to_json f)
       ])

let of_json j =
  { cross_zone_load_balancing =
      Aws.Util.option_map
        (Aws.Json.lookup j "CrossZoneLoadBalancing")
        CrossZoneLoadBalancing.of_json
  ; access_log = Aws.Util.option_map (Aws.Json.lookup j "AccessLog") AccessLog.of_json
  ; connection_draining =
      Aws.Util.option_map
        (Aws.Json.lookup j "ConnectionDraining")
        ConnectionDraining.of_json
  ; connection_settings =
      Aws.Util.option_map
        (Aws.Json.lookup j "ConnectionSettings")
        ConnectionSettings.of_json
  ; additional_attributes =
      AdditionalAttributes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdditionalAttributes"))
  }
