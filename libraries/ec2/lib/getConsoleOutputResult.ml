open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; timestamp : DateTime.t option
  ; output : String.t option
  }

let make ?instance_id ?timestamp ?output () = { instance_id; timestamp; output }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; timestamp = Aws.Util.option_bind (Aws.Xml.member "timestamp" xml) DateTime.parse
    ; output = Aws.Util.option_bind (Aws.Xml.member "output" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.output (fun f ->
             Aws.Query.Pair ("Output", String.to_query f))
       ; Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.output (fun f -> "output", String.to_json f)
       ; Aws.Util.option_map v.timestamp (fun f -> "timestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "timestamp") DateTime.of_json
  ; output = Aws.Util.option_map (Aws.Json.lookup j "output") String.of_json
  }
