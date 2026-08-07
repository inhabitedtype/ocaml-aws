type t = { cancelled_spot_instance_requests : CancelledSpotInstanceRequestList.t }

let make ?(cancelled_spot_instance_requests = []) () =
  { cancelled_spot_instance_requests }

let parse xml =
  Some
    { cancelled_spot_instance_requests =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "spotInstanceRequestSet" xml)
             CancelledSpotInstanceRequestList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SpotInstanceRequestSet"
              , CancelledSpotInstanceRequestList.to_query
                  v.cancelled_spot_instance_requests ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "spotInstanceRequestSet"
           , CancelledSpotInstanceRequestList.to_json v.cancelled_spot_instance_requests
           )
       ])

let of_json j =
  { cancelled_spot_instance_requests =
      CancelledSpotInstanceRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "spotInstanceRequestSet"))
  }
