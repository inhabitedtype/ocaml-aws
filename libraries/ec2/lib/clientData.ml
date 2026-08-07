open Aws.BaseTypes

type t =
  { comment : String.t option
  ; upload_end : DateTime.t option
  ; upload_size : Double.t option
  ; upload_start : DateTime.t option
  }

let make ?comment ?upload_end ?upload_size ?upload_start () =
  { comment; upload_end; upload_size; upload_start }

let parse xml =
  Some
    { comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
    ; upload_end = Aws.Util.option_bind (Aws.Xml.member "UploadEnd" xml) DateTime.parse
    ; upload_size = Aws.Util.option_bind (Aws.Xml.member "UploadSize" xml) Double.parse
    ; upload_start =
        Aws.Util.option_bind (Aws.Xml.member "UploadStart" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.upload_start (fun f ->
             Aws.Query.Pair ("UploadStart", DateTime.to_query f))
       ; Aws.Util.option_map v.upload_size (fun f ->
             Aws.Query.Pair ("UploadSize", Double.to_query f))
       ; Aws.Util.option_map v.upload_end (fun f ->
             Aws.Query.Pair ("UploadEnd", DateTime.to_query f))
       ; Aws.Util.option_map v.comment (fun f ->
             Aws.Query.Pair ("Comment", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.upload_start (fun f -> "UploadStart", DateTime.to_json f)
       ; Aws.Util.option_map v.upload_size (fun f -> "UploadSize", Double.to_json f)
       ; Aws.Util.option_map v.upload_end (fun f -> "UploadEnd", DateTime.to_json f)
       ; Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
       ])

let of_json j =
  { comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
  ; upload_end = Aws.Util.option_map (Aws.Json.lookup j "UploadEnd") DateTime.of_json
  ; upload_size = Aws.Util.option_map (Aws.Json.lookup j "UploadSize") Double.of_json
  ; upload_start = Aws.Util.option_map (Aws.Json.lookup j "UploadStart") DateTime.of_json
  }
