open Aws.BaseTypes

type t =
  { type_ : ThirdPartyType.t option
  ; arn : String.t option
  ; type_name : String.t option
  ; public_version_number : String.t option
  }

let make ?type_ ?arn ?type_name ?public_version_number () =
  { type_; arn; type_name; public_version_number }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) ThirdPartyType.parse
    ; arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; public_version_number =
        Aws.Util.option_bind (Aws.Xml.member "PublicVersionNumber" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_version_number (fun f ->
             Aws.Query.Pair ("PublicVersionNumber", String.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", ThirdPartyType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_version_number (fun f ->
             "PublicVersionNumber", String.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", ThirdPartyType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") ThirdPartyType.of_json
  ; arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; public_version_number =
      Aws.Util.option_map (Aws.Json.lookup j "PublicVersionNumber") String.of_json
  }
