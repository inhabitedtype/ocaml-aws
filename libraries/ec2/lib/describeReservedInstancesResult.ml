type t = { reserved_instances : ReservedInstancesList.t }

let make ?(reserved_instances = []) () = { reserved_instances }

let parse xml =
  Some
    { reserved_instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "reservedInstancesSet" xml)
             ReservedInstancesList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ReservedInstancesSet", ReservedInstancesList.to_query v.reserved_instances))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("reservedInstancesSet", ReservedInstancesList.to_json v.reserved_instances)
       ])

let of_json j =
  { reserved_instances =
      ReservedInstancesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "reservedInstancesSet"))
  }
