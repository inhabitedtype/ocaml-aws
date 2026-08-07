open Aws.BaseTypes

type t =
  { dimensions : StringList.t
  ; group : String.t option
  ; limit : Integer.t option
  }

let make ?(dimensions = []) ?group ?limit () = { dimensions; group; limit }

let parse xml =
  Some
    { dimensions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) StringList.parse)
    ; group = Aws.Util.option_bind (Aws.Xml.member "Group" xml) String.parse
    ; limit = Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.limit (fun f ->
             Aws.Query.Pair ("Limit", Integer.to_query f))
       ; Aws.Util.option_map v.group (fun f ->
             Aws.Query.Pair ("Group", String.to_query f))
       ; Some (Aws.Query.Pair ("Dimensions.member", StringList.to_query v.dimensions))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
       ; Aws.Util.option_map v.group (fun f -> "Group", String.to_json f)
       ; Some ("Dimensions", StringList.to_json v.dimensions)
       ])

let of_json j =
  { dimensions =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
  ; group = Aws.Util.option_map (Aws.Json.lookup j "Group") String.of_json
  ; limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
  }
