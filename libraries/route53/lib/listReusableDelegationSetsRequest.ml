open Aws.BaseTypes

type t =
  { marker : String.t option
  ; max_items : String.t option
  }

let make ?marker ?max_items () = { marker; max_items }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "marker" xml) String.parse
    ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f ->
             Aws.Query.Pair ("maxitems", String.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "marker") String.of_json
  ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
  }
