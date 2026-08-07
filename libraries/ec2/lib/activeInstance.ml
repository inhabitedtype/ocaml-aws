open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; instance_type : String.t option
  ; spot_instance_request_id : String.t option
  ; instance_health : InstanceHealthStatus.t option
  }

let make ?instance_id ?instance_type ?spot_instance_request_id ?instance_health () =
  { instance_id; instance_type; spot_instance_request_id; instance_health }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; spot_instance_request_id =
        Aws.Util.option_bind (Aws.Xml.member "spotInstanceRequestId" xml) String.parse
    ; instance_health =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceHealth" xml)
          InstanceHealthStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_health (fun f ->
             Aws.Query.Pair ("InstanceHealth", InstanceHealthStatus.to_query f))
       ; Aws.Util.option_map v.spot_instance_request_id (fun f ->
             Aws.Query.Pair ("SpotInstanceRequestId", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_health (fun f ->
             "instanceHealth", InstanceHealthStatus.to_json f)
       ; Aws.Util.option_map v.spot_instance_request_id (fun f ->
             "spotInstanceRequestId", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; spot_instance_request_id =
      Aws.Util.option_map (Aws.Json.lookup j "spotInstanceRequestId") String.of_json
  ; instance_health =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceHealth")
        InstanceHealthStatus.of_json
  }
