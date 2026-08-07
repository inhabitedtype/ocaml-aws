open Aws.BaseTypes

type t =
  { return : Boolean.t option
  ; group_id : String.t option
  }

let make ?return ?group_id () = { return; group_id }

let parse xml =
  Some
    { return = Aws.Util.option_bind (Aws.Xml.member "return" xml) Boolean.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.return (fun f ->
             Aws.Query.Pair ("Return", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_id (fun f -> "groupId", String.to_json f)
       ; Aws.Util.option_map v.return (fun f -> "return", Boolean.to_json f)
       ])

let of_json j =
  { return = Aws.Util.option_map (Aws.Json.lookup j "return") Boolean.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "groupId") String.of_json
  }
