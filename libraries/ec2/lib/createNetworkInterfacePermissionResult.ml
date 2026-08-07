type t = { interface_permission : NetworkInterfacePermission.t option }

let make ?interface_permission () = { interface_permission }

let parse xml =
  Some
    { interface_permission =
        Aws.Util.option_bind
          (Aws.Xml.member "interfacePermission" xml)
          NetworkInterfacePermission.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.interface_permission (fun f ->
             Aws.Query.Pair ("InterfacePermission", NetworkInterfacePermission.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.interface_permission (fun f ->
             "interfacePermission", NetworkInterfacePermission.to_json f)
       ])

let of_json j =
  { interface_permission =
      Aws.Util.option_map
        (Aws.Json.lookup j "interfacePermission")
        NetworkInterfacePermission.of_json
  }
