open Aws.BaseTypes

type t =
  { id : String.t
  ; caller_reference : String.t
  ; linked_service : LinkedService.t option
  ; health_check_config : HealthCheckConfig.t
  ; health_check_version : Long.t
  ; cloud_watch_alarm_configuration : CloudWatchAlarmConfiguration.t option
  }

let make
    ~id
    ~caller_reference
    ?linked_service
    ~health_check_config
    ~health_check_version
    ?cloud_watch_alarm_configuration
    () =
  { id
  ; caller_reference
  ; linked_service
  ; health_check_config
  ; health_check_version
  ; cloud_watch_alarm_configuration
  }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; caller_reference =
        Aws.Xml.required
          "CallerReference"
          (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
    ; linked_service =
        Aws.Util.option_bind (Aws.Xml.member "LinkedService" xml) LinkedService.parse
    ; health_check_config =
        Aws.Xml.required
          "HealthCheckConfig"
          (Aws.Util.option_bind
             (Aws.Xml.member "HealthCheckConfig" xml)
             HealthCheckConfig.parse)
    ; health_check_version =
        Aws.Xml.required
          "HealthCheckVersion"
          (Aws.Util.option_bind (Aws.Xml.member "HealthCheckVersion" xml) Long.parse)
    ; cloud_watch_alarm_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "CloudWatchAlarmConfiguration" xml)
          CloudWatchAlarmConfiguration.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cloud_watch_alarm_configuration (fun f ->
             Aws.Query.Pair
               ("CloudWatchAlarmConfiguration", CloudWatchAlarmConfiguration.to_query f))
       ; Some
           (Aws.Query.Pair ("HealthCheckVersion", Long.to_query v.health_check_version))
       ; Some
           (Aws.Query.Pair
              ("HealthCheckConfig", HealthCheckConfig.to_query v.health_check_config))
       ; Aws.Util.option_map v.linked_service (fun f ->
             Aws.Query.Pair ("LinkedService", LinkedService.to_query f))
       ; Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cloud_watch_alarm_configuration (fun f ->
             "CloudWatchAlarmConfiguration", CloudWatchAlarmConfiguration.to_json f)
       ; Some ("HealthCheckVersion", Long.to_json v.health_check_version)
       ; Some ("HealthCheckConfig", HealthCheckConfig.to_json v.health_check_config)
       ; Aws.Util.option_map v.linked_service (fun f ->
             "LinkedService", LinkedService.to_json f)
       ; Some ("CallerReference", String.to_json v.caller_reference)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; caller_reference =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
  ; linked_service =
      Aws.Util.option_map (Aws.Json.lookup j "LinkedService") LinkedService.of_json
  ; health_check_config =
      HealthCheckConfig.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckConfig"))
  ; health_check_version =
      Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheckVersion"))
  ; cloud_watch_alarm_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "CloudWatchAlarmConfiguration")
        CloudWatchAlarmConfiguration.of_json
  }
