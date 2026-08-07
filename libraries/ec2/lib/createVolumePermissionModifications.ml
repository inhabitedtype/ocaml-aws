type t =
  { add : CreateVolumePermissionList.t
  ; remove : CreateVolumePermissionList.t
  }

let make ?(add = []) ?(remove = []) () = { add; remove }

let parse xml =
  Some
    { add =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Add" xml)
             CreateVolumePermissionList.parse)
    ; remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Remove" xml)
             CreateVolumePermissionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Remove", CreateVolumePermissionList.to_query v.remove))
       ; Some (Aws.Query.Pair ("Add", CreateVolumePermissionList.to_query v.add))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Remove", CreateVolumePermissionList.to_json v.remove)
       ; Some ("Add", CreateVolumePermissionList.to_json v.add)
       ])

let of_json j =
  { add =
      CreateVolumePermissionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Add"))
  ; remove =
      CreateVolumePermissionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Remove"))
  }
