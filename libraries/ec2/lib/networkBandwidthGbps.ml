open Aws.BaseTypes

type t =
  { min : Double.t option
  ; max : Double.t option
  }

let make ?min ?max () = { min; max }

let parse xml =
  Some
    { min = Aws.Util.option_bind (Aws.Xml.member "min" xml) Double.parse
    ; max = Aws.Util.option_bind (Aws.Xml.member "max" xml) Double.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max (fun f -> Aws.Query.Pair ("Max", Double.to_query f))
       ; Aws.Util.option_map v.min (fun f -> Aws.Query.Pair ("Min", Double.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max (fun f -> "max", Double.to_json f)
       ; Aws.Util.option_map v.min (fun f -> "min", Double.to_json f)
       ])

let of_json j =
  { min = Aws.Util.option_map (Aws.Json.lookup j "min") Double.of_json
  ; max = Aws.Util.option_map (Aws.Json.lookup j "max") Double.of_json
  }
