type t = { associated_roles : AssociatedRolesList.t }

let make ?(associated_roles = []) () = { associated_roles }

let parse xml =
  Some
    { associated_roles =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "associatedRoleSet" xml)
             AssociatedRolesList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AssociatedRoleSet", AssociatedRolesList.to_query v.associated_roles))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("associatedRoleSet", AssociatedRolesList.to_json v.associated_roles) ])

let of_json j =
  { associated_roles =
      AssociatedRolesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "associatedRoleSet"))
  }
