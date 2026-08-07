open Aws.BaseTypes

type t =
  { limit : ReusableDelegationSetLimit.t
  ; count : Long.t
  }

let make ~limit ~count () = { limit; count }

let parse xml =
  Some
    { limit =
        Aws.Xml.required
          "Limit"
          (Aws.Util.option_bind
             (Aws.Xml.member "Limit" xml)
             ReusableDelegationSetLimit.parse)
    ; count =
        Aws.Xml.required
          "Count"
          (Aws.Util.option_bind (Aws.Xml.member "Count" xml) Long.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Count", Long.to_query v.count))
       ; Some (Aws.Query.Pair ("Limit", ReusableDelegationSetLimit.to_query v.limit))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Count", Long.to_json v.count)
       ; Some ("Limit", ReusableDelegationSetLimit.to_json v.limit)
       ])

let of_json j =
  { limit =
      ReusableDelegationSetLimit.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Limit"))
  ; count = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Count"))
  }
