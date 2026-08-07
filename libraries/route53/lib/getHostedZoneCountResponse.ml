open Aws.BaseTypes

type t = { hosted_zone_count : Long.t }

let make ~hosted_zone_count () = { hosted_zone_count }

let parse xml =
  Some
    { hosted_zone_count =
        Aws.Xml.required
          "HostedZoneCount"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZoneCount" xml) Long.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("HostedZoneCount", Long.to_query v.hosted_zone_count)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("HostedZoneCount", Long.to_json v.hosted_zone_count) ])

let of_json j =
  { hosted_zone_count =
      Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneCount"))
  }
