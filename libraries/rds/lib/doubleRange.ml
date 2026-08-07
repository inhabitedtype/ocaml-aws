open Aws.BaseTypes

type t =
  { from : Double.t option
  ; to_ : Double.t option
  }

let make ?from ?to_ () = { from; to_ }

let parse xml =
  Some
    { from = Aws.Util.option_bind (Aws.Xml.member "From" xml) Double.parse
    ; to_ = Aws.Util.option_bind (Aws.Xml.member "To" xml) Double.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.to_ (fun f -> Aws.Query.Pair ("To", Double.to_query f))
       ; Aws.Util.option_map v.from (fun f -> Aws.Query.Pair ("From", Double.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.to_ (fun f -> "To", Double.to_json f)
       ; Aws.Util.option_map v.from (fun f -> "From", Double.to_json f)
       ])

let of_json j =
  { from = Aws.Util.option_map (Aws.Json.lookup j "From") Double.of_json
  ; to_ = Aws.Util.option_map (Aws.Json.lookup j "To") Double.of_json
  }
