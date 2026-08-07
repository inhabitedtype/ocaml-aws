type t = { geo_location_details : GeoLocationDetails.t }

let make ~geo_location_details () = { geo_location_details }

let parse xml =
  Some
    { geo_location_details =
        Aws.Xml.required
          "GeoLocationDetails"
          (Aws.Util.option_bind
             (Aws.Xml.member "GeoLocationDetails" xml)
             GeoLocationDetails.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("GeoLocationDetails", GeoLocationDetails.to_query v.geo_location_details))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("GeoLocationDetails", GeoLocationDetails.to_json v.geo_location_details) ])

let of_json j =
  { geo_location_details =
      GeoLocationDetails.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GeoLocationDetails"))
  }
