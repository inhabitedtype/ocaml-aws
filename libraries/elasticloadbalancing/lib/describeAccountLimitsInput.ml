open Aws.BaseTypes

type t =
  { marker : String.t option
  ; page_size : Integer.t option
  }

let make ?marker ?page_size () = { marker; page_size }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; page_size = Aws.Util.option_bind (Aws.Xml.member "PageSize" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.page_size (fun f ->
             Aws.Query.Pair ("PageSize", Integer.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.page_size (fun f -> "PageSize", Integer.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; page_size = Aws.Util.option_map (Aws.Json.lookup j "PageSize") Integer.of_json
  }
