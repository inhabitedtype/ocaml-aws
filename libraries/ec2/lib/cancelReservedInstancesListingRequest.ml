open Aws.BaseTypes

type t = { reserved_instances_listing_id : String.t }

let make ~reserved_instances_listing_id () = { reserved_instances_listing_id }

let parse xml =
  Some
    { reserved_instances_listing_id =
        Aws.Xml.required
          "reservedInstancesListingId"
          (Aws.Util.option_bind
             (Aws.Xml.member "reservedInstancesListingId" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ReservedInstancesListingId"
              , String.to_query v.reserved_instances_listing_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("reservedInstancesListingId", String.to_json v.reserved_instances_listing_id)
       ])

let of_json j =
  { reserved_instances_listing_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "reservedInstancesListingId"))
  }
