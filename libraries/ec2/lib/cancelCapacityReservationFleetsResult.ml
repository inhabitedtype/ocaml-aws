type t =
  { successful_fleet_cancellations : CapacityReservationFleetCancellationStateSet.t
  ; failed_fleet_cancellations : FailedCapacityReservationFleetCancellationResultSet.t
  }

let make ?(successful_fleet_cancellations = []) ?(failed_fleet_cancellations = []) () =
  { successful_fleet_cancellations; failed_fleet_cancellations }

let parse xml =
  Some
    { successful_fleet_cancellations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "successfulFleetCancellationSet" xml)
             CapacityReservationFleetCancellationStateSet.parse)
    ; failed_fleet_cancellations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "failedFleetCancellationSet" xml)
             FailedCapacityReservationFleetCancellationResultSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "FailedFleetCancellationSet"
              , FailedCapacityReservationFleetCancellationResultSet.to_query
                  v.failed_fleet_cancellations ))
       ; Some
           (Aws.Query.Pair
              ( "SuccessfulFleetCancellationSet"
              , CapacityReservationFleetCancellationStateSet.to_query
                  v.successful_fleet_cancellations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "failedFleetCancellationSet"
           , FailedCapacityReservationFleetCancellationResultSet.to_json
               v.failed_fleet_cancellations )
       ; Some
           ( "successfulFleetCancellationSet"
           , CapacityReservationFleetCancellationStateSet.to_json
               v.successful_fleet_cancellations )
       ])

let of_json j =
  { successful_fleet_cancellations =
      CapacityReservationFleetCancellationStateSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "successfulFleetCancellationSet"))
  ; failed_fleet_cancellations =
      FailedCapacityReservationFleetCancellationResultSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "failedFleetCancellationSet"))
  }
