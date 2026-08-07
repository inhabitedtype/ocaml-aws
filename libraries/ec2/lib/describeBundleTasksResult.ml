type t = { bundle_tasks : BundleTaskList.t }

let make ?(bundle_tasks = []) () = { bundle_tasks }

let parse xml =
  Some
    { bundle_tasks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "bundleInstanceTasksSet" xml)
             BundleTaskList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("BundleInstanceTasksSet", BundleTaskList.to_query v.bundle_tasks))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("bundleInstanceTasksSet", BundleTaskList.to_json v.bundle_tasks) ])

let of_json j =
  { bundle_tasks =
      BundleTaskList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "bundleInstanceTasksSet"))
  }
