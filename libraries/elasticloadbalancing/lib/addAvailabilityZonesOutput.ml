type t = { availability_zones : AvailabilityZones.t }

let make ?(availability_zones = []) () = { availability_zones }

let parse xml =
  Some
    { availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZones" xml)
             AvailabilityZones.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AvailabilityZones.member", AvailabilityZones.to_query v.availability_zones))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones) ])

let of_json j =
  { availability_zones =
      AvailabilityZones.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
  }
