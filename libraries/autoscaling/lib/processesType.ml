type t = { processes : Processes.t }

let make ?(processes = []) () = { processes }

let parse xml =
  Some
    { processes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Processes" xml) Processes.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Processes.member", Processes.to_query v.processes)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Processes", Processes.to_json v.processes) ])

let of_json j =
  { processes = Processes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Processes"))
  }
