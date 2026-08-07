type t = { bundle_task : BundleTask.t option }

let make ?bundle_task () = { bundle_task }

let parse xml =
  Some
    { bundle_task =
        Aws.Util.option_bind (Aws.Xml.member "bundleInstanceTask" xml) BundleTask.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bundle_task (fun f ->
             Aws.Query.Pair ("BundleInstanceTask", BundleTask.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bundle_task (fun f ->
             "bundleInstanceTask", BundleTask.to_json f)
       ])

let of_json j =
  { bundle_task =
      Aws.Util.option_map (Aws.Json.lookup j "bundleInstanceTask") BundleTask.of_json
  }
