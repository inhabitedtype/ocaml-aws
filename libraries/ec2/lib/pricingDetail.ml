open Aws.BaseTypes

type t =
  { count : Integer.t option
  ; price : Double.t option
  }

let make ?count ?price () = { count; price }

let parse xml =
  Some
    { count = Aws.Util.option_bind (Aws.Xml.member "count" xml) Integer.parse
    ; price = Aws.Util.option_bind (Aws.Xml.member "price" xml) Double.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.price (fun f ->
             Aws.Query.Pair ("Price", Double.to_query f))
       ; Aws.Util.option_map v.count (fun f ->
             Aws.Query.Pair ("Count", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.price (fun f -> "price", Double.to_json f)
       ; Aws.Util.option_map v.count (fun f -> "count", Integer.to_json f)
       ])

let of_json j =
  { count = Aws.Util.option_map (Aws.Json.lookup j "count") Integer.of_json
  ; price = Aws.Util.option_map (Aws.Json.lookup j "price") Double.of_json
  }
