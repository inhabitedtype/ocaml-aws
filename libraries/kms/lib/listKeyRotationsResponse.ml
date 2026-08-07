open Aws.BaseTypes

type t =
  { rotations : RotationsList.t
  ; next_marker : String.t option
  ; truncated : Boolean.t option
  }

let make ?(rotations = []) ?next_marker ?truncated () =
  { rotations; next_marker; truncated }

let parse xml =
  Some
    { rotations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Rotations" xml) RotationsList.parse)
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
       ; Some (Aws.Query.Pair ("Rotations.member", RotationsList.to_query v.rotations))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.truncated (fun f -> "Truncated", Boolean.to_json f)
       ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
       ; Some ("Rotations", RotationsList.to_json v.rotations)
       ])

let of_json j =
  { rotations =
      RotationsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Rotations"))
  ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
  ; truncated = Aws.Util.option_map (Aws.Json.lookup j "Truncated") Boolean.of_json
  }
