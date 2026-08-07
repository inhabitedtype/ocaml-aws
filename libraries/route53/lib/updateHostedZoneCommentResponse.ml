type t = { hosted_zone : HostedZone.t }

let make ~hosted_zone () = { hosted_zone }

let parse xml =
  Some
    { hosted_zone =
        Aws.Xml.required
          "HostedZone"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZone" xml) HostedZone.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("HostedZone", HostedZone.to_query v.hosted_zone)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("HostedZone", HostedZone.to_json v.hosted_zone) ])

let of_json j =
  { hosted_zone =
      HostedZone.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZone"))
  }
