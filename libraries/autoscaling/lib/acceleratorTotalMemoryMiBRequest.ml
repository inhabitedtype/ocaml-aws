open Aws.BaseTypes

type t =
  { min : Integer.t option
  ; max : Integer.t option
  }

let make ?min ?max () = { min; max }

let parse xml =
  Some
    { min = Aws.Util.option_bind (Aws.Xml.member "Min" xml) Integer.parse
    ; max = Aws.Util.option_bind (Aws.Xml.member "Max" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max (fun f -> Aws.Query.Pair ("Max", Integer.to_query f))
       ; Aws.Util.option_map v.min (fun f -> Aws.Query.Pair ("Min", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max (fun f -> "Max", Integer.to_json f)
       ; Aws.Util.option_map v.min (fun f -> "Min", Integer.to_json f)
       ])

let of_json j =
  { min = Aws.Util.option_map (Aws.Json.lookup j "Min") Integer.of_json
  ; max = Aws.Util.option_map (Aws.Json.lookup j "Max") Integer.of_json
  }
