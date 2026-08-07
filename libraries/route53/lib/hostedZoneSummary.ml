open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; name : String.t
  ; owner : HostedZoneOwner.t
  }

let make ~hosted_zone_id ~name ~owner () = { hosted_zone_id; name; owner }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "HostedZoneId"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
    ; name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; owner =
        Aws.Xml.required
          "Owner"
          (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) HostedZoneOwner.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Owner", HostedZoneOwner.to_query v.owner))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Owner", HostedZoneOwner.to_json v.owner)
       ; Some ("Name", String.to_json v.name)
       ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
  ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; owner = HostedZoneOwner.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Owner"))
  }
