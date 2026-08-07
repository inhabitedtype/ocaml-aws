open Aws.BaseTypes

type t =
  { min : Integer.t
  ; max : Integer.t option
  }

let make ~min ?max () = { min; max }

let parse xml =
  Some
    { min =
        Aws.Xml.required
          "Min"
          (Aws.Util.option_bind (Aws.Xml.member "Min" xml) Integer.parse)
    ; max = Aws.Util.option_bind (Aws.Xml.member "Max" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max (fun f -> Aws.Query.Pair ("Max", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Min", Integer.to_query v.min))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max (fun f -> "Max", Integer.to_json f)
       ; Some ("Min", Integer.to_json v.min)
       ])

let of_json j =
  { min = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Min"))
  ; max = Aws.Util.option_map (Aws.Json.lookup j "Max") Integer.of_json
  }
