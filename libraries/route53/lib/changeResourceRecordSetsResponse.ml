type t = { change_info : ChangeInfo.t }

let make ~change_info () = { change_info }

let parse xml =
  Some
    { change_info =
        Aws.Xml.required
          "ChangeInfo"
          (Aws.Util.option_bind (Aws.Xml.member "ChangeInfo" xml) ChangeInfo.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("ChangeInfo", ChangeInfo.to_json v.change_info) ])

let of_json j =
  { change_info =
      ChangeInfo.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeInfo"))
  }
