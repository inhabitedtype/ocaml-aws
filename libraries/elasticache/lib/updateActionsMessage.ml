open Aws.BaseTypes

type t =
  { marker : String.t option
  ; update_actions : UpdateActionList.t
  }

let make ?marker ?(update_actions = []) () = { marker; update_actions }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; update_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UpdateActions" xml)
             UpdateActionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("UpdateActions.member", UpdateActionList.to_query v.update_actions))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("UpdateActions", UpdateActionList.to_json v.update_actions)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; update_actions =
      UpdateActionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UpdateActions"))
  }
