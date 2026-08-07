type t = { target_groups : TargetGroups.t }

let make ?(target_groups = []) () = { target_groups }

let parse xml =
  Some
    { target_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "targetGroups" xml) TargetGroups.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TargetGroups", TargetGroups.to_query v.target_groups)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("targetGroups", TargetGroups.to_json v.target_groups) ])

let of_json j =
  { target_groups =
      TargetGroups.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "targetGroups"))
  }
