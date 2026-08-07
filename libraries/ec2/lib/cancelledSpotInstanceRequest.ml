open Aws.BaseTypes

type t =
  { spot_instance_request_id : String.t option
  ; state : CancelSpotInstanceRequestState.t option
  }

let make ?spot_instance_request_id ?state () = { spot_instance_request_id; state }

let parse xml =
  Some
    { spot_instance_request_id =
        Aws.Util.option_bind (Aws.Xml.member "spotInstanceRequestId" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          CancelSpotInstanceRequestState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", CancelSpotInstanceRequestState.to_query f))
       ; Aws.Util.option_map v.spot_instance_request_id (fun f ->
             Aws.Query.Pair ("SpotInstanceRequestId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             "state", CancelSpotInstanceRequestState.to_json f)
       ; Aws.Util.option_map v.spot_instance_request_id (fun f ->
             "spotInstanceRequestId", String.to_json f)
       ])

let of_json j =
  { spot_instance_request_id =
      Aws.Util.option_map (Aws.Json.lookup j "spotInstanceRequestId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        CancelSpotInstanceRequestState.of_json
  }
