type t = { lifecycle_hooks : LifecycleHooks.t }

let make ?(lifecycle_hooks = []) () = { lifecycle_hooks }

let parse xml =
  Some
    { lifecycle_hooks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LifecycleHooks" xml)
             LifecycleHooks.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("LifecycleHooks.member", LifecycleHooks.to_query v.lifecycle_hooks))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LifecycleHooks", LifecycleHooks.to_json v.lifecycle_hooks) ])

let of_json j =
  { lifecycle_hooks =
      LifecycleHooks.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHooks"))
  }
