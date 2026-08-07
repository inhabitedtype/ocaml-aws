type t =
  { successful_fleet_requests : CancelSpotFleetRequestsSuccessSet.t
  ; unsuccessful_fleet_requests : CancelSpotFleetRequestsErrorSet.t
  }

let make ?(successful_fleet_requests = []) ?(unsuccessful_fleet_requests = []) () =
  { successful_fleet_requests; unsuccessful_fleet_requests }

let parse xml =
  Some
    { successful_fleet_requests =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "successfulFleetRequestSet" xml)
             CancelSpotFleetRequestsSuccessSet.parse)
    ; unsuccessful_fleet_requests =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unsuccessfulFleetRequestSet" xml)
             CancelSpotFleetRequestsErrorSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "UnsuccessfulFleetRequestSet"
              , CancelSpotFleetRequestsErrorSet.to_query v.unsuccessful_fleet_requests ))
       ; Some
           (Aws.Query.Pair
              ( "SuccessfulFleetRequestSet"
              , CancelSpotFleetRequestsSuccessSet.to_query v.successful_fleet_requests ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "unsuccessfulFleetRequestSet"
           , CancelSpotFleetRequestsErrorSet.to_json v.unsuccessful_fleet_requests )
       ; Some
           ( "successfulFleetRequestSet"
           , CancelSpotFleetRequestsSuccessSet.to_json v.successful_fleet_requests )
       ])

let of_json j =
  { successful_fleet_requests =
      CancelSpotFleetRequestsSuccessSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "successfulFleetRequestSet"))
  ; unsuccessful_fleet_requests =
      CancelSpotFleetRequestsErrorSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "unsuccessfulFleetRequestSet"))
  }
