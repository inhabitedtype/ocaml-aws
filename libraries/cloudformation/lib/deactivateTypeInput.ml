open Aws.BaseTypes

type t =
  { type_name : String.t option
  ; type_ : ThirdPartyType.t option
  ; arn : String.t option
  }

let make ?type_name ?type_ ?arn () = { type_name; type_; arn }

let parse xml =
  Some
    { type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) ThirdPartyType.parse
    ; arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", ThirdPartyType.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", ThirdPartyType.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ])

let of_json j =
  { type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") ThirdPartyType.of_json
  ; arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
  }
