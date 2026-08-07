open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; cloud_watch_logs_log_group_arn : String.t
  }

let make ~hosted_zone_id ~cloud_watch_logs_log_group_arn () =
  { hosted_zone_id; cloud_watch_logs_log_group_arn }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "HostedZoneId"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
    ; cloud_watch_logs_log_group_arn =
        Aws.Xml.required
          "CloudWatchLogsLogGroupArn"
          (Aws.Util.option_bind
             (Aws.Xml.member "CloudWatchLogsLogGroupArn" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CloudWatchLogsLogGroupArn"
              , String.to_query v.cloud_watch_logs_log_group_arn ))
       ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("CloudWatchLogsLogGroupArn", String.to_json v.cloud_watch_logs_log_group_arn)
       ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
  ; cloud_watch_logs_log_group_arn =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CloudWatchLogsLogGroupArn"))
  }
