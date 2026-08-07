open Aws.BaseTypes

type t =
  { is_logging : Boolean.t option
  ; latest_delivery_error : String.t option
  ; latest_notification_error : String.t option
  ; latest_delivery_time : DateTime.t option
  ; latest_notification_time : DateTime.t option
  ; start_logging_time : DateTime.t option
  ; stop_logging_time : DateTime.t option
  ; latest_cloud_watch_logs_delivery_error : String.t option
  ; latest_cloud_watch_logs_delivery_time : DateTime.t option
  ; latest_digest_delivery_time : DateTime.t option
  ; latest_digest_delivery_error : String.t option
  ; latest_delivery_attempt_time : String.t option
  ; latest_notification_attempt_time : String.t option
  ; latest_notification_attempt_succeeded : String.t option
  ; latest_delivery_attempt_succeeded : String.t option
  ; time_logging_started : String.t option
  ; time_logging_stopped : String.t option
  }

let make
    ?is_logging
    ?latest_delivery_error
    ?latest_notification_error
    ?latest_delivery_time
    ?latest_notification_time
    ?start_logging_time
    ?stop_logging_time
    ?latest_cloud_watch_logs_delivery_error
    ?latest_cloud_watch_logs_delivery_time
    ?latest_digest_delivery_time
    ?latest_digest_delivery_error
    ?latest_delivery_attempt_time
    ?latest_notification_attempt_time
    ?latest_notification_attempt_succeeded
    ?latest_delivery_attempt_succeeded
    ?time_logging_started
    ?time_logging_stopped
    () =
  { is_logging
  ; latest_delivery_error
  ; latest_notification_error
  ; latest_delivery_time
  ; latest_notification_time
  ; start_logging_time
  ; stop_logging_time
  ; latest_cloud_watch_logs_delivery_error
  ; latest_cloud_watch_logs_delivery_time
  ; latest_digest_delivery_time
  ; latest_digest_delivery_error
  ; latest_delivery_attempt_time
  ; latest_notification_attempt_time
  ; latest_notification_attempt_succeeded
  ; latest_delivery_attempt_succeeded
  ; time_logging_started
  ; time_logging_stopped
  }

let parse xml =
  Some
    { is_logging = Aws.Util.option_bind (Aws.Xml.member "IsLogging" xml) Boolean.parse
    ; latest_delivery_error =
        Aws.Util.option_bind (Aws.Xml.member "LatestDeliveryError" xml) String.parse
    ; latest_notification_error =
        Aws.Util.option_bind (Aws.Xml.member "LatestNotificationError" xml) String.parse
    ; latest_delivery_time =
        Aws.Util.option_bind (Aws.Xml.member "LatestDeliveryTime" xml) DateTime.parse
    ; latest_notification_time =
        Aws.Util.option_bind (Aws.Xml.member "LatestNotificationTime" xml) DateTime.parse
    ; start_logging_time =
        Aws.Util.option_bind (Aws.Xml.member "StartLoggingTime" xml) DateTime.parse
    ; stop_logging_time =
        Aws.Util.option_bind (Aws.Xml.member "StopLoggingTime" xml) DateTime.parse
    ; latest_cloud_watch_logs_delivery_error =
        Aws.Util.option_bind
          (Aws.Xml.member "LatestCloudWatchLogsDeliveryError" xml)
          String.parse
    ; latest_cloud_watch_logs_delivery_time =
        Aws.Util.option_bind
          (Aws.Xml.member "LatestCloudWatchLogsDeliveryTime" xml)
          DateTime.parse
    ; latest_digest_delivery_time =
        Aws.Util.option_bind
          (Aws.Xml.member "LatestDigestDeliveryTime" xml)
          DateTime.parse
    ; latest_digest_delivery_error =
        Aws.Util.option_bind (Aws.Xml.member "LatestDigestDeliveryError" xml) String.parse
    ; latest_delivery_attempt_time =
        Aws.Util.option_bind (Aws.Xml.member "LatestDeliveryAttemptTime" xml) String.parse
    ; latest_notification_attempt_time =
        Aws.Util.option_bind
          (Aws.Xml.member "LatestNotificationAttemptTime" xml)
          String.parse
    ; latest_notification_attempt_succeeded =
        Aws.Util.option_bind
          (Aws.Xml.member "LatestNotificationAttemptSucceeded" xml)
          String.parse
    ; latest_delivery_attempt_succeeded =
        Aws.Util.option_bind
          (Aws.Xml.member "LatestDeliveryAttemptSucceeded" xml)
          String.parse
    ; time_logging_started =
        Aws.Util.option_bind (Aws.Xml.member "TimeLoggingStarted" xml) String.parse
    ; time_logging_stopped =
        Aws.Util.option_bind (Aws.Xml.member "TimeLoggingStopped" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.time_logging_stopped (fun f ->
             Aws.Query.Pair ("TimeLoggingStopped", String.to_query f))
       ; Aws.Util.option_map v.time_logging_started (fun f ->
             Aws.Query.Pair ("TimeLoggingStarted", String.to_query f))
       ; Aws.Util.option_map v.latest_delivery_attempt_succeeded (fun f ->
             Aws.Query.Pair ("LatestDeliveryAttemptSucceeded", String.to_query f))
       ; Aws.Util.option_map v.latest_notification_attempt_succeeded (fun f ->
             Aws.Query.Pair ("LatestNotificationAttemptSucceeded", String.to_query f))
       ; Aws.Util.option_map v.latest_notification_attempt_time (fun f ->
             Aws.Query.Pair ("LatestNotificationAttemptTime", String.to_query f))
       ; Aws.Util.option_map v.latest_delivery_attempt_time (fun f ->
             Aws.Query.Pair ("LatestDeliveryAttemptTime", String.to_query f))
       ; Aws.Util.option_map v.latest_digest_delivery_error (fun f ->
             Aws.Query.Pair ("LatestDigestDeliveryError", String.to_query f))
       ; Aws.Util.option_map v.latest_digest_delivery_time (fun f ->
             Aws.Query.Pair ("LatestDigestDeliveryTime", DateTime.to_query f))
       ; Aws.Util.option_map v.latest_cloud_watch_logs_delivery_time (fun f ->
             Aws.Query.Pair ("LatestCloudWatchLogsDeliveryTime", DateTime.to_query f))
       ; Aws.Util.option_map v.latest_cloud_watch_logs_delivery_error (fun f ->
             Aws.Query.Pair ("LatestCloudWatchLogsDeliveryError", String.to_query f))
       ; Aws.Util.option_map v.stop_logging_time (fun f ->
             Aws.Query.Pair ("StopLoggingTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_logging_time (fun f ->
             Aws.Query.Pair ("StartLoggingTime", DateTime.to_query f))
       ; Aws.Util.option_map v.latest_notification_time (fun f ->
             Aws.Query.Pair ("LatestNotificationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.latest_delivery_time (fun f ->
             Aws.Query.Pair ("LatestDeliveryTime", DateTime.to_query f))
       ; Aws.Util.option_map v.latest_notification_error (fun f ->
             Aws.Query.Pair ("LatestNotificationError", String.to_query f))
       ; Aws.Util.option_map v.latest_delivery_error (fun f ->
             Aws.Query.Pair ("LatestDeliveryError", String.to_query f))
       ; Aws.Util.option_map v.is_logging (fun f ->
             Aws.Query.Pair ("IsLogging", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.time_logging_stopped (fun f ->
             "TimeLoggingStopped", String.to_json f)
       ; Aws.Util.option_map v.time_logging_started (fun f ->
             "TimeLoggingStarted", String.to_json f)
       ; Aws.Util.option_map v.latest_delivery_attempt_succeeded (fun f ->
             "LatestDeliveryAttemptSucceeded", String.to_json f)
       ; Aws.Util.option_map v.latest_notification_attempt_succeeded (fun f ->
             "LatestNotificationAttemptSucceeded", String.to_json f)
       ; Aws.Util.option_map v.latest_notification_attempt_time (fun f ->
             "LatestNotificationAttemptTime", String.to_json f)
       ; Aws.Util.option_map v.latest_delivery_attempt_time (fun f ->
             "LatestDeliveryAttemptTime", String.to_json f)
       ; Aws.Util.option_map v.latest_digest_delivery_error (fun f ->
             "LatestDigestDeliveryError", String.to_json f)
       ; Aws.Util.option_map v.latest_digest_delivery_time (fun f ->
             "LatestDigestDeliveryTime", DateTime.to_json f)
       ; Aws.Util.option_map v.latest_cloud_watch_logs_delivery_time (fun f ->
             "LatestCloudWatchLogsDeliveryTime", DateTime.to_json f)
       ; Aws.Util.option_map v.latest_cloud_watch_logs_delivery_error (fun f ->
             "LatestCloudWatchLogsDeliveryError", String.to_json f)
       ; Aws.Util.option_map v.stop_logging_time (fun f ->
             "StopLoggingTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_logging_time (fun f ->
             "StartLoggingTime", DateTime.to_json f)
       ; Aws.Util.option_map v.latest_notification_time (fun f ->
             "LatestNotificationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.latest_delivery_time (fun f ->
             "LatestDeliveryTime", DateTime.to_json f)
       ; Aws.Util.option_map v.latest_notification_error (fun f ->
             "LatestNotificationError", String.to_json f)
       ; Aws.Util.option_map v.latest_delivery_error (fun f ->
             "LatestDeliveryError", String.to_json f)
       ; Aws.Util.option_map v.is_logging (fun f -> "IsLogging", Boolean.to_json f)
       ])

let of_json j =
  { is_logging = Aws.Util.option_map (Aws.Json.lookup j "IsLogging") Boolean.of_json
  ; latest_delivery_error =
      Aws.Util.option_map (Aws.Json.lookup j "LatestDeliveryError") String.of_json
  ; latest_notification_error =
      Aws.Util.option_map (Aws.Json.lookup j "LatestNotificationError") String.of_json
  ; latest_delivery_time =
      Aws.Util.option_map (Aws.Json.lookup j "LatestDeliveryTime") DateTime.of_json
  ; latest_notification_time =
      Aws.Util.option_map (Aws.Json.lookup j "LatestNotificationTime") DateTime.of_json
  ; start_logging_time =
      Aws.Util.option_map (Aws.Json.lookup j "StartLoggingTime") DateTime.of_json
  ; stop_logging_time =
      Aws.Util.option_map (Aws.Json.lookup j "StopLoggingTime") DateTime.of_json
  ; latest_cloud_watch_logs_delivery_error =
      Aws.Util.option_map
        (Aws.Json.lookup j "LatestCloudWatchLogsDeliveryError")
        String.of_json
  ; latest_cloud_watch_logs_delivery_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "LatestCloudWatchLogsDeliveryTime")
        DateTime.of_json
  ; latest_digest_delivery_time =
      Aws.Util.option_map (Aws.Json.lookup j "LatestDigestDeliveryTime") DateTime.of_json
  ; latest_digest_delivery_error =
      Aws.Util.option_map (Aws.Json.lookup j "LatestDigestDeliveryError") String.of_json
  ; latest_delivery_attempt_time =
      Aws.Util.option_map (Aws.Json.lookup j "LatestDeliveryAttemptTime") String.of_json
  ; latest_notification_attempt_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "LatestNotificationAttemptTime")
        String.of_json
  ; latest_notification_attempt_succeeded =
      Aws.Util.option_map
        (Aws.Json.lookup j "LatestNotificationAttemptSucceeded")
        String.of_json
  ; latest_delivery_attempt_succeeded =
      Aws.Util.option_map
        (Aws.Json.lookup j "LatestDeliveryAttemptSucceeded")
        String.of_json
  ; time_logging_started =
      Aws.Util.option_map (Aws.Json.lookup j "TimeLoggingStarted") String.of_json
  ; time_logging_stopped =
      Aws.Util.option_map (Aws.Json.lookup j "TimeLoggingStopped") String.of_json
  }
