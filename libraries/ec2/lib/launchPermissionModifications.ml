type t =
  { add : LaunchPermissionList.t
  ; remove : LaunchPermissionList.t
  }

let make ?(add = []) ?(remove = []) () = { add; remove }

let parse xml =
  Some
    { add =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Add" xml) LaunchPermissionList.parse)
    ; remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Remove" xml) LaunchPermissionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Remove", LaunchPermissionList.to_query v.remove))
       ; Some (Aws.Query.Pair ("Add", LaunchPermissionList.to_query v.add))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Remove", LaunchPermissionList.to_json v.remove)
       ; Some ("Add", LaunchPermissionList.to_json v.add)
       ])

let of_json j =
  { add = LaunchPermissionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Add"))
  ; remove =
      LaunchPermissionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Remove"))
  }
