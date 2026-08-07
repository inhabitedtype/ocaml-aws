open Aws.BaseTypes

type t =
  { caller_reference : String.t
  ; hosted_zone_id : String.t
  ; key_management_service_arn : String.t
  ; name : String.t
  ; status : String.t
  }

let make ~caller_reference ~hosted_zone_id ~key_management_service_arn ~name ~status () =
  { caller_reference; hosted_zone_id; key_management_service_arn; name; status }

let parse xml =
  Some
    { caller_reference =
        Aws.Xml.required
          "CallerReference"
          (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
    ; hosted_zone_id =
        Aws.Xml.required
          "HostedZoneId"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
    ; key_management_service_arn =
        Aws.Xml.required
          "KeyManagementServiceArn"
          (Aws.Util.option_bind
             (Aws.Xml.member "KeyManagementServiceArn" xml)
             String.parse)
    ; name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; status =
        Aws.Xml.required
          "Status"
          (Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Status", String.to_query v.status))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ; Some
           (Aws.Query.Pair
              ("KeyManagementServiceArn", String.to_query v.key_management_service_arn))
       ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
       ; Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Status", String.to_json v.status)
       ; Some ("Name", String.to_json v.name)
       ; Some ("KeyManagementServiceArn", String.to_json v.key_management_service_arn)
       ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
       ; Some ("CallerReference", String.to_json v.caller_reference)
       ])

let of_json j =
  { caller_reference =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
  ; hosted_zone_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
  ; key_management_service_arn =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyManagementServiceArn"))
  ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; status = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Status"))
  }
