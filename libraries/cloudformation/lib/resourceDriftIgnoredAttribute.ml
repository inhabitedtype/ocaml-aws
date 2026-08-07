open Aws.BaseTypes

type t =
  { path : String.t option
  ; reason : DriftIgnoredReason.t option
  }

let make ?path ?reason () = { path; reason }

let parse xml =
  Some
    { path = Aws.Util.option_bind (Aws.Xml.member "Path" xml) String.parse
    ; reason = Aws.Util.option_bind (Aws.Xml.member "Reason" xml) DriftIgnoredReason.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reason (fun f ->
             Aws.Query.Pair ("Reason", DriftIgnoredReason.to_query f))
       ; Aws.Util.option_map v.path (fun f -> Aws.Query.Pair ("Path", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reason (fun f -> "Reason", DriftIgnoredReason.to_json f)
       ; Aws.Util.option_map v.path (fun f -> "Path", String.to_json f)
       ])

let of_json j =
  { path = Aws.Util.option_map (Aws.Json.lookup j "Path") String.of_json
  ; reason = Aws.Util.option_map (Aws.Json.lookup j "Reason") DriftIgnoredReason.of_json
  }
