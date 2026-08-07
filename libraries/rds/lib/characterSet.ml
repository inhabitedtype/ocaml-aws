open Aws.BaseTypes

type t =
  { character_set_name : String.t option
  ; character_set_description : String.t option
  }

let make ?character_set_name ?character_set_description () =
  { character_set_name; character_set_description }

let parse xml =
  Some
    { character_set_name =
        Aws.Util.option_bind (Aws.Xml.member "CharacterSetName" xml) String.parse
    ; character_set_description =
        Aws.Util.option_bind (Aws.Xml.member "CharacterSetDescription" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.character_set_description (fun f ->
             Aws.Query.Pair ("CharacterSetDescription", String.to_query f))
       ; Aws.Util.option_map v.character_set_name (fun f ->
             Aws.Query.Pair ("CharacterSetName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.character_set_description (fun f ->
             "CharacterSetDescription", String.to_json f)
       ; Aws.Util.option_map v.character_set_name (fun f ->
             "CharacterSetName", String.to_json f)
       ])

let of_json j =
  { character_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "CharacterSetName") String.of_json
  ; character_set_description =
      Aws.Util.option_map (Aws.Json.lookup j "CharacterSetDescription") String.of_json
  }
