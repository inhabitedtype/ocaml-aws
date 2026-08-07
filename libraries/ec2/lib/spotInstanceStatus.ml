open Aws.BaseTypes

type t =
  { code : String.t option
  ; message : String.t option
  ; update_time : DateTime.t option
  }

let make ?code ?message ?update_time () = { code; message; update_time }

let parse xml =
  Some
    { code = Aws.Util.option_bind (Aws.Xml.member "code" xml) String.parse
    ; message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse
    ; update_time = Aws.Util.option_bind (Aws.Xml.member "updateTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.update_time (fun f ->
             Aws.Query.Pair ("UpdateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("Message", String.to_query f))
       ; Aws.Util.option_map v.code (fun f -> Aws.Query.Pair ("Code", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.update_time (fun f -> "updateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.message (fun f -> "message", String.to_json f)
       ; Aws.Util.option_map v.code (fun f -> "code", String.to_json f)
       ])

let of_json j =
  { code = Aws.Util.option_map (Aws.Json.lookup j "code") String.of_json
  ; message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json
  ; update_time = Aws.Util.option_map (Aws.Json.lookup j "updateTime") DateTime.of_json
  }
