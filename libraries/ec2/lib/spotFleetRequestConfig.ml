open Aws.BaseTypes

type t =
  { activity_status : ActivityStatus.t option
  ; create_time : DateTime.t option
  ; spot_fleet_request_config : SpotFleetRequestConfigData.t option
  ; spot_fleet_request_id : String.t option
  ; spot_fleet_request_state : BatchState.t option
  ; tags : TagList.t
  }

let make
    ?activity_status
    ?create_time
    ?spot_fleet_request_config
    ?spot_fleet_request_id
    ?spot_fleet_request_state
    ?(tags = [])
    () =
  { activity_status
  ; create_time
  ; spot_fleet_request_config
  ; spot_fleet_request_id
  ; spot_fleet_request_state
  ; tags
  }

let parse xml =
  Some
    { activity_status =
        Aws.Util.option_bind (Aws.Xml.member "activityStatus" xml) ActivityStatus.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; spot_fleet_request_config =
        Aws.Util.option_bind
          (Aws.Xml.member "spotFleetRequestConfig" xml)
          SpotFleetRequestConfigData.parse
    ; spot_fleet_request_id =
        Aws.Util.option_bind (Aws.Xml.member "spotFleetRequestId" xml) String.parse
    ; spot_fleet_request_state =
        Aws.Util.option_bind (Aws.Xml.member "spotFleetRequestState" xml) BatchState.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.spot_fleet_request_state (fun f ->
             Aws.Query.Pair ("SpotFleetRequestState", BatchState.to_query f))
       ; Aws.Util.option_map v.spot_fleet_request_id (fun f ->
             Aws.Query.Pair ("SpotFleetRequestId", String.to_query f))
       ; Aws.Util.option_map v.spot_fleet_request_config (fun f ->
             Aws.Query.Pair
               ("SpotFleetRequestConfig", SpotFleetRequestConfigData.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.activity_status (fun f ->
             Aws.Query.Pair ("ActivityStatus", ActivityStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.spot_fleet_request_state (fun f ->
             "spotFleetRequestState", BatchState.to_json f)
       ; Aws.Util.option_map v.spot_fleet_request_id (fun f ->
             "spotFleetRequestId", String.to_json f)
       ; Aws.Util.option_map v.spot_fleet_request_config (fun f ->
             "spotFleetRequestConfig", SpotFleetRequestConfigData.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ; Aws.Util.option_map v.activity_status (fun f ->
             "activityStatus", ActivityStatus.to_json f)
       ])

let of_json j =
  { activity_status =
      Aws.Util.option_map (Aws.Json.lookup j "activityStatus") ActivityStatus.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; spot_fleet_request_config =
      Aws.Util.option_map
        (Aws.Json.lookup j "spotFleetRequestConfig")
        SpotFleetRequestConfigData.of_json
  ; spot_fleet_request_id =
      Aws.Util.option_map (Aws.Json.lookup j "spotFleetRequestId") String.of_json
  ; spot_fleet_request_state =
      Aws.Util.option_map (Aws.Json.lookup j "spotFleetRequestState") BatchState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
