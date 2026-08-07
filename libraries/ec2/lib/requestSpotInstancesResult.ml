type t = { spot_instance_requests : SpotInstanceRequestList.t }

let make ?(spot_instance_requests = []) () = { spot_instance_requests }

let parse xml =
  Some
    { spot_instance_requests =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "spotInstanceRequestSet" xml)
             SpotInstanceRequestList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SpotInstanceRequestSet"
              , SpotInstanceRequestList.to_query v.spot_instance_requests ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "spotInstanceRequestSet"
           , SpotInstanceRequestList.to_json v.spot_instance_requests )
       ])

let of_json j =
  { spot_instance_requests =
      SpotInstanceRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "spotInstanceRequestSet"))
  }
