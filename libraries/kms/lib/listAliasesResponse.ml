open Aws.BaseTypes

type t =
  { aliases : AliasList.t
  ; next_marker : String.t option
  ; truncated : Boolean.t option
  }

let make ?(aliases = []) ?next_marker ?truncated () = { aliases; next_marker; truncated }

let parse xml =
  Some
    { aliases =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Aliases" xml) AliasList.parse)
    ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
    ; truncated = Aws.Util.option_bind (Aws.Xml.member "Truncated" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.truncated (fun f ->
             Aws.Query.Pair ("Truncated", Boolean.to_query f))
       ; Aws.Util.option_map v.next_marker (fun f ->
             Aws.Query.Pair ("NextMarker", String.to_query f))
       ; Some (Aws.Query.Pair ("Aliases.member", AliasList.to_query v.aliases))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.truncated (fun f -> "Truncated", Boolean.to_json f)
       ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
       ; Some ("Aliases", AliasList.to_json v.aliases)
       ])

let of_json j =
  { aliases = AliasList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Aliases"))
  ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
  ; truncated = Aws.Util.option_map (Aws.Json.lookup j "Truncated") Boolean.of_json
  }
