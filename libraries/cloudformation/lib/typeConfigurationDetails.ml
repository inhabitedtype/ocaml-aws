open Aws.BaseTypes

type t =
  { arn : String.t option
  ; alias : String.t option
  ; configuration : String.t option
  ; last_updated : DateTime.t option
  ; type_arn : String.t option
  ; type_name : String.t option
  ; is_default_configuration : Boolean.t option
  }

let make
    ?arn
    ?alias
    ?configuration
    ?last_updated
    ?type_arn
    ?type_name
    ?is_default_configuration
    () =
  { arn
  ; alias
  ; configuration
  ; last_updated
  ; type_arn
  ; type_name
  ; is_default_configuration
  }

let parse xml =
  Some
    { arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
    ; alias = Aws.Util.option_bind (Aws.Xml.member "Alias" xml) String.parse
    ; configuration =
        Aws.Util.option_bind (Aws.Xml.member "Configuration" xml) String.parse
    ; last_updated =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdated" xml) DateTime.parse
    ; type_arn = Aws.Util.option_bind (Aws.Xml.member "TypeArn" xml) String.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; is_default_configuration =
        Aws.Util.option_bind (Aws.Xml.member "IsDefaultConfiguration" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_default_configuration (fun f ->
             Aws.Query.Pair ("IsDefaultConfiguration", Boolean.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.type_arn (fun f ->
             Aws.Query.Pair ("TypeArn", String.to_query f))
       ; Aws.Util.option_map v.last_updated (fun f ->
             Aws.Query.Pair ("LastUpdated", DateTime.to_query f))
       ; Aws.Util.option_map v.configuration (fun f ->
             Aws.Query.Pair ("Configuration", String.to_query f))
       ; Aws.Util.option_map v.alias (fun f ->
             Aws.Query.Pair ("Alias", String.to_query f))
       ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_default_configuration (fun f ->
             "IsDefaultConfiguration", Boolean.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.type_arn (fun f -> "TypeArn", String.to_json f)
       ; Aws.Util.option_map v.last_updated (fun f -> "LastUpdated", DateTime.to_json f)
       ; Aws.Util.option_map v.configuration (fun f -> "Configuration", String.to_json f)
       ; Aws.Util.option_map v.alias (fun f -> "Alias", String.to_json f)
       ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
       ])

let of_json j =
  { arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
  ; alias = Aws.Util.option_map (Aws.Json.lookup j "Alias") String.of_json
  ; configuration = Aws.Util.option_map (Aws.Json.lookup j "Configuration") String.of_json
  ; last_updated = Aws.Util.option_map (Aws.Json.lookup j "LastUpdated") DateTime.of_json
  ; type_arn = Aws.Util.option_map (Aws.Json.lookup j "TypeArn") String.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; is_default_configuration =
      Aws.Util.option_map (Aws.Json.lookup j "IsDefaultConfiguration") Boolean.of_json
  }
