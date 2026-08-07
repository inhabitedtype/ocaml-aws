type t =
  { details : VolumeStatusDetailsList.t
  ; status : VolumeStatusInfoStatus.t option
  }

let make ?(details = []) ?status () = { details; status }

let parse xml =
  Some
    { details =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "details" xml)
             VolumeStatusDetailsList.parse)
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) VolumeStatusInfoStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", VolumeStatusInfoStatus.to_query f))
       ; Some (Aws.Query.Pair ("Details", VolumeStatusDetailsList.to_query v.details))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             "status", VolumeStatusInfoStatus.to_json f)
       ; Some ("details", VolumeStatusDetailsList.to_json v.details)
       ])

let of_json j =
  { details =
      VolumeStatusDetailsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "details"))
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "status") VolumeStatusInfoStatus.of_json
  }
