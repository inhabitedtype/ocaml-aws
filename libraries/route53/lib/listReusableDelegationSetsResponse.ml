open Aws.BaseTypes

type t =
  { delegation_sets : DelegationSets.t
  ; marker : String.t
  ; is_truncated : Boolean.t
  ; next_marker : String.t option
  ; max_items : String.t
  }

let make ~delegation_sets ~marker ~is_truncated ?next_marker ~max_items () =
  { delegation_sets; marker; is_truncated; next_marker; max_items }

let parse xml =
  Some
    { delegation_sets =
        Aws.Xml.required
          "DelegationSets"
          (Aws.Util.option_bind
             (Aws.Xml.member "DelegationSets" xml)
             DelegationSets.parse)
    ; marker =
        Aws.Xml.required
          "Marker"
          (Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse)
    ; is_truncated =
        Aws.Xml.required
          "IsTruncated"
          (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
    ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
    ; max_items =
        Aws.Xml.required
          "MaxItems"
          (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
       ; Aws.Util.option_map v.next_marker (fun f ->
             Aws.Query.Pair ("NextMarker", String.to_query f))
       ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
       ; Some (Aws.Query.Pair ("Marker", String.to_query v.marker))
       ; Some
           (Aws.Query.Pair
              ("DelegationSets.member", DelegationSets.to_query v.delegation_sets))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MaxItems", String.to_json v.max_items)
       ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
       ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
       ; Some ("Marker", String.to_json v.marker)
       ; Some ("DelegationSets", DelegationSets.to_json v.delegation_sets)
       ])

let of_json j =
  { delegation_sets =
      DelegationSets.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DelegationSets"))
  ; marker = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Marker"))
  ; is_truncated =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
  ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
  ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
  }
