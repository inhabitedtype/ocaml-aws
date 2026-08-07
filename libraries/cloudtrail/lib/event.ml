open Aws.BaseTypes

type t =
  { event_id : String.t option
  ; event_name : String.t option
  ; read_only : String.t option
  ; access_key_id : String.t option
  ; event_time : DateTime.t option
  ; event_source : String.t option
  ; username : String.t option
  ; resources : ResourceList.t
  ; cloud_trail_event : String.t option
  }

let make
    ?event_id
    ?event_name
    ?read_only
    ?access_key_id
    ?event_time
    ?event_source
    ?username
    ?(resources = [])
    ?cloud_trail_event
    () =
  { event_id
  ; event_name
  ; read_only
  ; access_key_id
  ; event_time
  ; event_source
  ; username
  ; resources
  ; cloud_trail_event
  }

let parse xml =
  Some
    { event_id = Aws.Util.option_bind (Aws.Xml.member "EventId" xml) String.parse
    ; event_name = Aws.Util.option_bind (Aws.Xml.member "EventName" xml) String.parse
    ; read_only = Aws.Util.option_bind (Aws.Xml.member "ReadOnly" xml) String.parse
    ; access_key_id = Aws.Util.option_bind (Aws.Xml.member "AccessKeyId" xml) String.parse
    ; event_time = Aws.Util.option_bind (Aws.Xml.member "EventTime" xml) DateTime.parse
    ; event_source = Aws.Util.option_bind (Aws.Xml.member "EventSource" xml) String.parse
    ; username = Aws.Util.option_bind (Aws.Xml.member "Username" xml) String.parse
    ; resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Resources" xml) ResourceList.parse)
    ; cloud_trail_event =
        Aws.Util.option_bind (Aws.Xml.member "CloudTrailEvent" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cloud_trail_event (fun f ->
             Aws.Query.Pair ("CloudTrailEvent", String.to_query f))
       ; Some (Aws.Query.Pair ("Resources.member", ResourceList.to_query v.resources))
       ; Aws.Util.option_map v.username (fun f ->
             Aws.Query.Pair ("Username", String.to_query f))
       ; Aws.Util.option_map v.event_source (fun f ->
             Aws.Query.Pair ("EventSource", String.to_query f))
       ; Aws.Util.option_map v.event_time (fun f ->
             Aws.Query.Pair ("EventTime", DateTime.to_query f))
       ; Aws.Util.option_map v.access_key_id (fun f ->
             Aws.Query.Pair ("AccessKeyId", String.to_query f))
       ; Aws.Util.option_map v.read_only (fun f ->
             Aws.Query.Pair ("ReadOnly", String.to_query f))
       ; Aws.Util.option_map v.event_name (fun f ->
             Aws.Query.Pair ("EventName", String.to_query f))
       ; Aws.Util.option_map v.event_id (fun f ->
             Aws.Query.Pair ("EventId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cloud_trail_event (fun f ->
             "CloudTrailEvent", String.to_json f)
       ; Some ("Resources", ResourceList.to_json v.resources)
       ; Aws.Util.option_map v.username (fun f -> "Username", String.to_json f)
       ; Aws.Util.option_map v.event_source (fun f -> "EventSource", String.to_json f)
       ; Aws.Util.option_map v.event_time (fun f -> "EventTime", DateTime.to_json f)
       ; Aws.Util.option_map v.access_key_id (fun f -> "AccessKeyId", String.to_json f)
       ; Aws.Util.option_map v.read_only (fun f -> "ReadOnly", String.to_json f)
       ; Aws.Util.option_map v.event_name (fun f -> "EventName", String.to_json f)
       ; Aws.Util.option_map v.event_id (fun f -> "EventId", String.to_json f)
       ])

let of_json j =
  { event_id = Aws.Util.option_map (Aws.Json.lookup j "EventId") String.of_json
  ; event_name = Aws.Util.option_map (Aws.Json.lookup j "EventName") String.of_json
  ; read_only = Aws.Util.option_map (Aws.Json.lookup j "ReadOnly") String.of_json
  ; access_key_id = Aws.Util.option_map (Aws.Json.lookup j "AccessKeyId") String.of_json
  ; event_time = Aws.Util.option_map (Aws.Json.lookup j "EventTime") DateTime.of_json
  ; event_source = Aws.Util.option_map (Aws.Json.lookup j "EventSource") String.of_json
  ; username = Aws.Util.option_map (Aws.Json.lookup j "Username") String.of_json
  ; resources =
      ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Resources"))
  ; cloud_trail_event =
      Aws.Util.option_map (Aws.Json.lookup j "CloudTrailEvent") String.of_json
  }
