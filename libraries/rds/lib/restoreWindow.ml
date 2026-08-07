open Aws.BaseTypes

type t =
  { earliest_time : DateTime.t option
  ; latest_time : DateTime.t option
  }

let make ?earliest_time ?latest_time () = { earliest_time; latest_time }

let parse xml =
  Some
    { earliest_time =
        Aws.Util.option_bind (Aws.Xml.member "EarliestTime" xml) DateTime.parse
    ; latest_time = Aws.Util.option_bind (Aws.Xml.member "LatestTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.latest_time (fun f ->
             Aws.Query.Pair ("LatestTime", DateTime.to_query f))
       ; Aws.Util.option_map v.earliest_time (fun f ->
             Aws.Query.Pair ("EarliestTime", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.latest_time (fun f -> "LatestTime", DateTime.to_json f)
       ; Aws.Util.option_map v.earliest_time (fun f -> "EarliestTime", DateTime.to_json f)
       ])

let of_json j =
  { earliest_time =
      Aws.Util.option_map (Aws.Json.lookup j "EarliestTime") DateTime.of_json
  ; latest_time = Aws.Util.option_map (Aws.Json.lookup j "LatestTime") DateTime.of_json
  }
