open Aws.BaseTypes

type t =
  { timestamp : DateTime.t
  ; approximate_value : Double.t
  }

let make ~timestamp ~approximate_value () = { timestamp; approximate_value }

let parse xml =
  Some
    { timestamp =
        Aws.Xml.required
          "Timestamp"
          (Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse)
    ; approximate_value =
        Aws.Xml.required
          "ApproximateValue"
          (Aws.Util.option_bind (Aws.Xml.member "ApproximateValue" xml) Double.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ApproximateValue", Double.to_query v.approximate_value))
       ; Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ApproximateValue", Double.to_json v.approximate_value)
       ; Some ("Timestamp", DateTime.to_json v.timestamp)
       ])

let of_json j =
  { timestamp = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
  ; approximate_value =
      Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApproximateValue"))
  }
