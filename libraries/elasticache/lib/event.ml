open Aws.BaseTypes

type t =
  { source_identifier : String.t option
  ; source_type : SourceType.t option
  ; message : String.t option
  ; date : DateTime.t option
  }

let make ?source_identifier ?source_type ?message ?date () =
  { source_identifier; source_type; message; date }

let parse xml =
  Some
    { source_identifier =
        Aws.Util.option_bind (Aws.Xml.member "SourceIdentifier" xml) String.parse
    ; source_type =
        Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) SourceType.parse
    ; message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse
    ; date = Aws.Util.option_bind (Aws.Xml.member "Date" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.date (fun f ->
             Aws.Query.Pair ("Date", DateTime.to_query f))
       ; Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("Message", String.to_query f))
       ; Aws.Util.option_map v.source_type (fun f ->
             Aws.Query.Pair ("SourceType", SourceType.to_query f))
       ; Aws.Util.option_map v.source_identifier (fun f ->
             Aws.Query.Pair ("SourceIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.date (fun f -> "Date", DateTime.to_json f)
       ; Aws.Util.option_map v.message (fun f -> "Message", String.to_json f)
       ; Aws.Util.option_map v.source_type (fun f -> "SourceType", SourceType.to_json f)
       ; Aws.Util.option_map v.source_identifier (fun f ->
             "SourceIdentifier", String.to_json f)
       ])

let of_json j =
  { source_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "SourceIdentifier") String.of_json
  ; source_type = Aws.Util.option_map (Aws.Json.lookup j "SourceType") SourceType.of_json
  ; message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json
  ; date = Aws.Util.option_map (Aws.Json.lookup j "Date") DateTime.of_json
  }
