open Aws.BaseTypes

type t =
  { from : Integer.t option
  ; to_ : Integer.t option
  }

let make ?from ?to_ () = { from; to_ }

let parse xml =
  Some
    { from = Aws.Util.option_bind (Aws.Xml.member "from" xml) Integer.parse
    ; to_ = Aws.Util.option_bind (Aws.Xml.member "to" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.to_ (fun f -> Aws.Query.Pair ("To", Integer.to_query f))
       ; Aws.Util.option_map v.from (fun f -> Aws.Query.Pair ("From", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.to_ (fun f -> "to", Integer.to_json f)
       ; Aws.Util.option_map v.from (fun f -> "from", Integer.to_json f)
       ])

let of_json j =
  { from = Aws.Util.option_map (Aws.Json.lookup j "from") Integer.of_json
  ; to_ = Aws.Util.option_map (Aws.Json.lookup j "to") Integer.of_json
  }
