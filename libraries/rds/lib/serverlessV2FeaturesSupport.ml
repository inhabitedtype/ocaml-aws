open Aws.BaseTypes

type t =
  { min_capacity : Double.t option
  ; max_capacity : Double.t option
  }

let make ?min_capacity ?max_capacity () = { min_capacity; max_capacity }

let parse xml =
  Some
    { min_capacity = Aws.Util.option_bind (Aws.Xml.member "MinCapacity" xml) Double.parse
    ; max_capacity = Aws.Util.option_bind (Aws.Xml.member "MaxCapacity" xml) Double.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_capacity (fun f ->
             Aws.Query.Pair ("MaxCapacity", Double.to_query f))
       ; Aws.Util.option_map v.min_capacity (fun f ->
             Aws.Query.Pair ("MinCapacity", Double.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_capacity (fun f -> "MaxCapacity", Double.to_json f)
       ; Aws.Util.option_map v.min_capacity (fun f -> "MinCapacity", Double.to_json f)
       ])

let of_json j =
  { min_capacity = Aws.Util.option_map (Aws.Json.lookup j "MinCapacity") Double.of_json
  ; max_capacity = Aws.Util.option_map (Aws.Json.lookup j "MaxCapacity") Double.of_json
  }
