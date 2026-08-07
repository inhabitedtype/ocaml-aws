open Aws.BaseTypes

type t =
  { description : String.t option
  ; event_id : String.t option
  ; event_type : String.t option
  ; not_after : DateTime.t option
  ; not_before : DateTime.t option
  ; instance_id : String.t option
  }

let make ?description ?event_id ?event_type ?not_after ?not_before ?instance_id () =
  { description; event_id; event_type; not_after; not_before; instance_id }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; event_id = Aws.Util.option_bind (Aws.Xml.member "eventId" xml) String.parse
    ; event_type = Aws.Util.option_bind (Aws.Xml.member "eventType" xml) String.parse
    ; not_after = Aws.Util.option_bind (Aws.Xml.member "notAfter" xml) DateTime.parse
    ; not_before = Aws.Util.option_bind (Aws.Xml.member "notBefore" xml) DateTime.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.not_before (fun f ->
             Aws.Query.Pair ("NotBefore", DateTime.to_query f))
       ; Aws.Util.option_map v.not_after (fun f ->
             Aws.Query.Pair ("NotAfter", DateTime.to_query f))
       ; Aws.Util.option_map v.event_type (fun f ->
             Aws.Query.Pair ("EventType", String.to_query f))
       ; Aws.Util.option_map v.event_id (fun f ->
             Aws.Query.Pair ("EventId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.not_before (fun f -> "notBefore", DateTime.to_json f)
       ; Aws.Util.option_map v.not_after (fun f -> "notAfter", DateTime.to_json f)
       ; Aws.Util.option_map v.event_type (fun f -> "eventType", String.to_json f)
       ; Aws.Util.option_map v.event_id (fun f -> "eventId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; event_id = Aws.Util.option_map (Aws.Json.lookup j "eventId") String.of_json
  ; event_type = Aws.Util.option_map (Aws.Json.lookup j "eventType") String.of_json
  ; not_after = Aws.Util.option_map (Aws.Json.lookup j "notAfter") DateTime.of_json
  ; not_before = Aws.Util.option_map (Aws.Json.lookup j "notBefore") DateTime.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  }
