open Aws.BaseTypes

type t =
  { event_description : String.t option
  ; event_sub_type : String.t option
  ; instance_id : String.t option
  }

let make ?event_description ?event_sub_type ?instance_id () =
  { event_description; event_sub_type; instance_id }

let parse xml =
  Some
    { event_description =
        Aws.Util.option_bind (Aws.Xml.member "eventDescription" xml) String.parse
    ; event_sub_type =
        Aws.Util.option_bind (Aws.Xml.member "eventSubType" xml) String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.event_sub_type (fun f ->
             Aws.Query.Pair ("EventSubType", String.to_query f))
       ; Aws.Util.option_map v.event_description (fun f ->
             Aws.Query.Pair ("EventDescription", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.event_sub_type (fun f -> "eventSubType", String.to_json f)
       ; Aws.Util.option_map v.event_description (fun f ->
             "eventDescription", String.to_json f)
       ])

let of_json j =
  { event_description =
      Aws.Util.option_map (Aws.Json.lookup j "eventDescription") String.of_json
  ; event_sub_type = Aws.Util.option_map (Aws.Json.lookup j "eventSubType") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  }
