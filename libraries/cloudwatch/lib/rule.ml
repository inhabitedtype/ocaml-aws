type t = { schedule : Schedule.t }

let make ~schedule () = { schedule }

let parse xml =
  Some
    { schedule =
        Aws.Xml.required
          "Schedule"
          (Aws.Util.option_bind (Aws.Xml.member "Schedule" xml) Schedule.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Schedule", Schedule.to_query v.schedule)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Schedule", Schedule.to_json v.schedule) ])

let of_json j =
  { schedule = Schedule.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Schedule")) }
