type t =
  { add : LoadPermissionListRequest.t
  ; remove : LoadPermissionListRequest.t
  }

let make ?(add = []) ?(remove = []) () = { add; remove }

let parse xml =
  Some
    { add =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Add" xml)
             LoadPermissionListRequest.parse)
    ; remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Remove" xml)
             LoadPermissionListRequest.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Remove", LoadPermissionListRequest.to_query v.remove))
       ; Some (Aws.Query.Pair ("Add", LoadPermissionListRequest.to_query v.add))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Remove", LoadPermissionListRequest.to_json v.remove)
       ; Some ("Add", LoadPermissionListRequest.to_json v.add)
       ])

let of_json j =
  { add =
      LoadPermissionListRequest.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Add"))
  ; remove =
      LoadPermissionListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Remove"))
  }
