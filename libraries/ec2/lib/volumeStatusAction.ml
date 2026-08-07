open Aws.BaseTypes

type t =
  { code : String.t option
  ; description : String.t option
  ; event_id : String.t option
  ; event_type : String.t option
  }

let make ?code ?description ?event_id ?event_type () =
  { code; description; event_id; event_type }

let parse xml =
  Some
    { code = Aws.Util.option_bind (Aws.Xml.member "code" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; event_id = Aws.Util.option_bind (Aws.Xml.member "eventId" xml) String.parse
    ; event_type = Aws.Util.option_bind (Aws.Xml.member "eventType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_type (fun f ->
             Aws.Query.Pair ("EventType", String.to_query f))
       ; Aws.Util.option_map v.event_id (fun f ->
             Aws.Query.Pair ("EventId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.code (fun f -> Aws.Query.Pair ("Code", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.event_type (fun f -> "eventType", String.to_json f)
       ; Aws.Util.option_map v.event_id (fun f -> "eventId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.code (fun f -> "code", String.to_json f)
       ])

let of_json j =
  { code = Aws.Util.option_map (Aws.Json.lookup j "code") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; event_id = Aws.Util.option_map (Aws.Json.lookup j "eventId") String.of_json
  ; event_type = Aws.Util.option_map (Aws.Json.lookup j "eventType") String.of_json
  }
