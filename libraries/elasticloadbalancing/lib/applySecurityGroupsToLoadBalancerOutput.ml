type t = { security_groups : SecurityGroups.t }

let make ?(security_groups = []) () = { security_groups }

let parse xml =
  Some
    { security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroups" xml)
             SecurityGroups.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("SecurityGroups.member", SecurityGroups.to_query v.security_groups))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SecurityGroups", SecurityGroups.to_json v.security_groups) ])

let of_json j =
  { security_groups =
      SecurityGroups.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
  }
