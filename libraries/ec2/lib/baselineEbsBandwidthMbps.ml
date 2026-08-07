open Aws.BaseTypes

type t =
  { min : Integer.t option
  ; max : Integer.t option
  }

let make ?min ?max () = { min; max }

let parse xml =
  Some
    { min = Aws.Util.option_bind (Aws.Xml.member "min" xml) Integer.parse
    ; max = Aws.Util.option_bind (Aws.Xml.member "max" xml) Integer.parse
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
       [ Aws.Util.option_map v.max (fun f -> "max", Integer.to_json f)
       ; Aws.Util.option_map v.min (fun f -> "min", Integer.to_json f)
       ])

let of_json j =
  { min = Aws.Util.option_map (Aws.Json.lookup j "min") Integer.of_json
  ; max = Aws.Util.option_map (Aws.Json.lookup j "max") Integer.of_json
  }
