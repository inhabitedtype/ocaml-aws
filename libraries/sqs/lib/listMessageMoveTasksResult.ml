type t = { results : ListMessageMoveTasksResultEntryList.t }

let make ?(results = []) () = { results }

let parse xml =
  Some { results = Aws.Util.of_option [] (ListMessageMoveTasksResultEntryList.parse xml) }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Results.member", ListMessageMoveTasksResultEntryList.to_query v.results))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Results", ListMessageMoveTasksResultEntryList.to_json v.results) ])

let of_json j =
  { results =
      ListMessageMoveTasksResultEntryList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Results"))
  }
