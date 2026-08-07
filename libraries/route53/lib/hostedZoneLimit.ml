open Aws.BaseTypes

type t =
  { type_ : HostedZoneLimitType.t
  ; value : Long.t
  }

let make ~type_ ~value () = { type_; value }

let parse xml =
  Some
    { type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) HostedZoneLimitType.parse)
    ; value =
        Aws.Xml.required
          "Value"
          (Aws.Util.option_bind (Aws.Xml.member "Value" xml) Long.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Value", Long.to_query v.value))
       ; Some (Aws.Query.Pair ("Type", HostedZoneLimitType.to_query v.type_))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Value", Long.to_json v.value)
       ; Some ("Type", HostedZoneLimitType.to_json v.type_)
       ])

let of_json j =
  { type_ =
      HostedZoneLimitType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; value = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
  }
