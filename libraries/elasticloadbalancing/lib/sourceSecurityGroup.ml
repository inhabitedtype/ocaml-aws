open Aws.BaseTypes

type t =
  { owner_alias : String.t option
  ; group_name : String.t option
  }

let make ?owner_alias ?group_name () = { owner_alias; group_name }

let parse xml =
  Some
    { owner_alias = Aws.Util.option_bind (Aws.Xml.member "OwnerAlias" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "GroupName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.owner_alias (fun f ->
             Aws.Query.Pair ("OwnerAlias", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_name (fun f -> "GroupName", String.to_json f)
       ; Aws.Util.option_map v.owner_alias (fun f -> "OwnerAlias", String.to_json f)
       ])

let of_json j =
  { owner_alias = Aws.Util.option_map (Aws.Json.lookup j "OwnerAlias") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "GroupName") String.of_json
  }
