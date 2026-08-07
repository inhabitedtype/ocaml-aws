type t = { activities : Activities.t }

let make ?(activities = []) () = { activities }

let parse xml =
  Some
    { activities =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Activities" xml) Activities.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Activities.member", Activities.to_query v.activities)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("Activities", Activities.to_json v.activities) ])

let of_json j =
  { activities =
      Activities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Activities"))
  }
