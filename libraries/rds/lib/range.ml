open Aws.BaseTypes

type t =
  { from : Integer.t option
  ; to_ : Integer.t option
  ; step : Integer.t option
  }

let make ?from ?to_ ?step () = { from; to_; step }

let parse xml =
  Some
    { from = Aws.Util.option_bind (Aws.Xml.member "From" xml) Integer.parse
    ; to_ = Aws.Util.option_bind (Aws.Xml.member "To" xml) Integer.parse
    ; step = Aws.Util.option_bind (Aws.Xml.member "Step" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.step (fun f -> Aws.Query.Pair ("Step", Integer.to_query f))
       ; Aws.Util.option_map v.to_ (fun f -> Aws.Query.Pair ("To", Integer.to_query f))
       ; Aws.Util.option_map v.from (fun f -> Aws.Query.Pair ("From", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.step (fun f -> "Step", Integer.to_json f)
       ; Aws.Util.option_map v.to_ (fun f -> "To", Integer.to_json f)
       ; Aws.Util.option_map v.from (fun f -> "From", Integer.to_json f)
       ])

let of_json j =
  { from = Aws.Util.option_map (Aws.Json.lookup j "From") Integer.of_json
  ; to_ = Aws.Util.option_map (Aws.Json.lookup j "To") Integer.of_json
  ; step = Aws.Util.option_map (Aws.Json.lookup j "Step") Integer.of_json
  }
