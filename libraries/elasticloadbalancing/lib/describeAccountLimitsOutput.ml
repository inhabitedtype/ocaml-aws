open Aws.BaseTypes

type t =
  { limits : Limits.t
  ; next_marker : String.t option
  }

let make ?(limits = []) ?next_marker () = { limits; next_marker }

let parse xml =
  Some
    { limits =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Limits" xml) Limits.parse)
    ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_marker (fun f ->
             Aws.Query.Pair ("NextMarker", String.to_query f))
       ; Some (Aws.Query.Pair ("Limits.member", Limits.to_query v.limits))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
       ; Some ("Limits", Limits.to_json v.limits)
       ])

let of_json j =
  { limits = Limits.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Limits"))
  ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
  }
