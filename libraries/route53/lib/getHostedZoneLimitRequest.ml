open Aws.BaseTypes

type t =
  { type_ : HostedZoneLimitType.t
  ; hosted_zone_id : String.t
  }

let make ~type_ ~hosted_zone_id () = { type_; hosted_zone_id }

let parse xml =
  Some
    { type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) HostedZoneLimitType.parse)
    ; hosted_zone_id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
       ; Some (Aws.Query.Pair ("Type", HostedZoneLimitType.to_query v.type_))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Id", String.to_json v.hosted_zone_id)
       ; Some ("Type", HostedZoneLimitType.to_json v.type_)
       ])

let of_json j =
  { type_ =
      HostedZoneLimitType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  }
