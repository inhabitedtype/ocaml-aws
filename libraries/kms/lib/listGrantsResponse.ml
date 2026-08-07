open Aws.BaseTypes

type t =
  { grants : GrantList.t
  ; next_marker : String.t option
  ; truncated : Boolean.t option
  }

let make ?(grants = []) ?next_marker ?truncated () = { grants; next_marker; truncated }

let parse xml =
  Some
    { grants =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Grants" xml) GrantList.parse)
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
       ; Some (Aws.Query.Pair ("Grants.member", GrantList.to_query v.grants))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.truncated (fun f -> "Truncated", Boolean.to_json f)
       ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
       ; Some ("Grants", GrantList.to_json v.grants)
       ])

let of_json j =
  { grants = GrantList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Grants"))
  ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
  ; truncated = Aws.Util.option_map (Aws.Json.lookup j "Truncated") Boolean.of_json
  }
