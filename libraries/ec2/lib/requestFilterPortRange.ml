open Aws.BaseTypes

type t =
  { from_port : Integer.t option
  ; to_port : Integer.t option
  }

let make ?from_port ?to_port () = { from_port; to_port }

let parse xml =
  Some
    { from_port = Aws.Util.option_bind (Aws.Xml.member "FromPort" xml) Integer.parse
    ; to_port = Aws.Util.option_bind (Aws.Xml.member "ToPort" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.to_port (fun f ->
             Aws.Query.Pair ("ToPort", Integer.to_query f))
       ; Aws.Util.option_map v.from_port (fun f ->
             Aws.Query.Pair ("FromPort", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.to_port (fun f -> "ToPort", Integer.to_json f)
       ; Aws.Util.option_map v.from_port (fun f -> "FromPort", Integer.to_json f)
       ])

let of_json j =
  { from_port = Aws.Util.option_map (Aws.Json.lookup j "FromPort") Integer.of_json
  ; to_port = Aws.Util.option_map (Aws.Json.lookup j "ToPort") Integer.of_json
  }
