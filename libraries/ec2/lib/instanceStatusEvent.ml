open Aws.BaseTypes

type t =
  { instance_event_id : String.t option
  ; code : EventCode.t option
  ; description : String.t option
  ; not_after : DateTime.t option
  ; not_before : DateTime.t option
  ; not_before_deadline : DateTime.t option
  }

let make
    ?instance_event_id
    ?code
    ?description
    ?not_after
    ?not_before
    ?not_before_deadline
    () =
  { instance_event_id; code; description; not_after; not_before; not_before_deadline }

let parse xml =
  Some
    { instance_event_id =
        Aws.Util.option_bind (Aws.Xml.member "instanceEventId" xml) String.parse
    ; code = Aws.Util.option_bind (Aws.Xml.member "code" xml) EventCode.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; not_after = Aws.Util.option_bind (Aws.Xml.member "notAfter" xml) DateTime.parse
    ; not_before = Aws.Util.option_bind (Aws.Xml.member "notBefore" xml) DateTime.parse
    ; not_before_deadline =
        Aws.Util.option_bind (Aws.Xml.member "notBeforeDeadline" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.not_before_deadline (fun f ->
             Aws.Query.Pair ("NotBeforeDeadline", DateTime.to_query f))
       ; Aws.Util.option_map v.not_before (fun f ->
             Aws.Query.Pair ("NotBefore", DateTime.to_query f))
       ; Aws.Util.option_map v.not_after (fun f ->
             Aws.Query.Pair ("NotAfter", DateTime.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.code (fun f ->
             Aws.Query.Pair ("Code", EventCode.to_query f))
       ; Aws.Util.option_map v.instance_event_id (fun f ->
             Aws.Query.Pair ("InstanceEventId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.not_before_deadline (fun f ->
             "notBeforeDeadline", DateTime.to_json f)
       ; Aws.Util.option_map v.not_before (fun f -> "notBefore", DateTime.to_json f)
       ; Aws.Util.option_map v.not_after (fun f -> "notAfter", DateTime.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.code (fun f -> "code", EventCode.to_json f)
       ; Aws.Util.option_map v.instance_event_id (fun f ->
             "instanceEventId", String.to_json f)
       ])

let of_json j =
  { instance_event_id =
      Aws.Util.option_map (Aws.Json.lookup j "instanceEventId") String.of_json
  ; code = Aws.Util.option_map (Aws.Json.lookup j "code") EventCode.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; not_after = Aws.Util.option_map (Aws.Json.lookup j "notAfter") DateTime.of_json
  ; not_before = Aws.Util.option_map (Aws.Json.lookup j "notBefore") DateTime.of_json
  ; not_before_deadline =
      Aws.Util.option_map (Aws.Json.lookup j "notBeforeDeadline") DateTime.of_json
  }
