open Aws.BaseTypes

type t = { maximum_days_since_created : Integer.t option }

let make ?maximum_days_since_created () = { maximum_days_since_created }

let parse xml =
  Some
    { maximum_days_since_created =
        Aws.Util.option_bind (Aws.Xml.member "maximumDaysSinceCreated" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_days_since_created (fun f ->
             Aws.Query.Pair ("MaximumDaysSinceCreated", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_days_since_created (fun f ->
             "maximumDaysSinceCreated", Integer.to_json f)
       ])

let of_json j =
  { maximum_days_since_created =
      Aws.Util.option_map (Aws.Json.lookup j "maximumDaysSinceCreated") Integer.of_json
  }
