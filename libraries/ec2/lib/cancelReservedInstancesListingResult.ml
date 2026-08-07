type t = { reserved_instances_listings : ReservedInstancesListingList.t }

let make ?(reserved_instances_listings = []) () = { reserved_instances_listings }

let parse xml =
  Some
    { reserved_instances_listings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "reservedInstancesListingsSet" xml)
             ReservedInstancesListingList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ReservedInstancesListingsSet"
              , ReservedInstancesListingList.to_query v.reserved_instances_listings ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "reservedInstancesListingsSet"
           , ReservedInstancesListingList.to_json v.reserved_instances_listings )
       ])

let of_json j =
  { reserved_instances_listings =
      ReservedInstancesListingList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "reservedInstancesListingsSet"))
  }
