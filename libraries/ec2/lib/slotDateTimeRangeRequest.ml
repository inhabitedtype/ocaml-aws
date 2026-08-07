open Aws.BaseTypes

type t =
  { earliest_time : DateTime.t
  ; latest_time : DateTime.t
  }

let make ~earliest_time ~latest_time () = { earliest_time; latest_time }

let parse xml =
  Some
    { earliest_time =
        Aws.Xml.required
          "EarliestTime"
          (Aws.Util.option_bind (Aws.Xml.member "EarliestTime" xml) DateTime.parse)
    ; latest_time =
        Aws.Xml.required
          "LatestTime"
          (Aws.Util.option_bind (Aws.Xml.member "LatestTime" xml) DateTime.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("LatestTime", DateTime.to_query v.latest_time))
       ; Some (Aws.Query.Pair ("EarliestTime", DateTime.to_query v.earliest_time))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LatestTime", DateTime.to_json v.latest_time)
       ; Some ("EarliestTime", DateTime.to_json v.earliest_time)
       ])

let of_json j =
  { earliest_time =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EarliestTime"))
  ; latest_time =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LatestTime"))
  }
