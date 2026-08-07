open Aws.BaseTypes

type t =
  { start_time : DateTime.t
  ; end_time : DateTime.t
  }

let make ~start_time ~end_time () = { start_time; end_time }

let parse xml =
  Some
    { start_time =
        Aws.Xml.required
          "StartTime"
          (Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse)
    ; end_time =
        Aws.Xml.required
          "EndTime"
          (Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("EndTime", DateTime.to_query v.end_time))
       ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("EndTime", DateTime.to_json v.end_time)
       ; Some ("StartTime", DateTime.to_json v.start_time)
       ])

let of_json j =
  { start_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
  ; end_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EndTime"))
  }
