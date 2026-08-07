type t =
  { successful_fleet_deletions : DeleteFleetSuccessSet.t
  ; unsuccessful_fleet_deletions : DeleteFleetErrorSet.t
  }

let make ?(successful_fleet_deletions = []) ?(unsuccessful_fleet_deletions = []) () =
  { successful_fleet_deletions; unsuccessful_fleet_deletions }

let parse xml =
  Some
    { successful_fleet_deletions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "successfulFleetDeletionSet" xml)
             DeleteFleetSuccessSet.parse)
    ; unsuccessful_fleet_deletions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unsuccessfulFleetDeletionSet" xml)
             DeleteFleetErrorSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "UnsuccessfulFleetDeletionSet"
              , DeleteFleetErrorSet.to_query v.unsuccessful_fleet_deletions ))
       ; Some
           (Aws.Query.Pair
              ( "SuccessfulFleetDeletionSet"
              , DeleteFleetSuccessSet.to_query v.successful_fleet_deletions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "unsuccessfulFleetDeletionSet"
           , DeleteFleetErrorSet.to_json v.unsuccessful_fleet_deletions )
       ; Some
           ( "successfulFleetDeletionSet"
           , DeleteFleetSuccessSet.to_json v.successful_fleet_deletions )
       ])

let of_json j =
  { successful_fleet_deletions =
      DeleteFleetSuccessSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "successfulFleetDeletionSet"))
  ; unsuccessful_fleet_deletions =
      DeleteFleetErrorSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "unsuccessfulFleetDeletionSet"))
  }
