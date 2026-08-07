open Aws.BaseTypes

type t = { maximum_days_since_deprecated : Integer.t option }

let make ?maximum_days_since_deprecated () = { maximum_days_since_deprecated }

let parse xml =
  Some
    { maximum_days_since_deprecated =
        Aws.Util.option_bind
          (Aws.Xml.member "MaximumDaysSinceDeprecated" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_days_since_deprecated (fun f ->
             Aws.Query.Pair ("MaximumDaysSinceDeprecated", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_days_since_deprecated (fun f ->
             "MaximumDaysSinceDeprecated", Integer.to_json f)
       ])

let of_json j =
  { maximum_days_since_deprecated =
      Aws.Util.option_map (Aws.Json.lookup j "MaximumDaysSinceDeprecated") Integer.of_json
  }
