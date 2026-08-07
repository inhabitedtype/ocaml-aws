open Aws.BaseTypes

type t =
  { status_type : String.t option
  ; normal : Boolean.t option
  ; status : String.t option
  ; message : String.t option
  }

let make ?status_type ?normal ?status ?message () =
  { status_type; normal; status; message }

let parse xml =
  Some
    { status_type = Aws.Util.option_bind (Aws.Xml.member "StatusType" xml) String.parse
    ; normal = Aws.Util.option_bind (Aws.Xml.member "Normal" xml) Boolean.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("Message", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.normal (fun f ->
             Aws.Query.Pair ("Normal", Boolean.to_query f))
       ; Aws.Util.option_map v.status_type (fun f ->
             Aws.Query.Pair ("StatusType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f -> "Message", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.normal (fun f -> "Normal", Boolean.to_json f)
       ; Aws.Util.option_map v.status_type (fun f -> "StatusType", String.to_json f)
       ])

let of_json j =
  { status_type = Aws.Util.option_map (Aws.Json.lookup j "StatusType") String.of_json
  ; normal = Aws.Util.option_map (Aws.Json.lookup j "Normal") Boolean.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json
  }
