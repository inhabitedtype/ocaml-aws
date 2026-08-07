open Aws.BaseTypes

type t =
  { id : String.t option
  ; type_ : String.t option
  ; description : String.t option
  ; is_excludable : Boolean.t option
  }

let make ?id ?type_ ?description ?is_excludable () =
  { id; type_; description; is_excludable }

let parse xml =
  Some
    { id = Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; is_excludable =
        Aws.Util.option_bind (Aws.Xml.member "isExcludable" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_excludable (fun f ->
             Aws.Query.Pair ("IsExcludable", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f -> Aws.Query.Pair ("Type", String.to_query f))
       ; Aws.Util.option_map v.id (fun f -> Aws.Query.Pair ("Id", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_excludable (fun f -> "isExcludable", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", String.to_json f)
       ; Aws.Util.option_map v.id (fun f -> "id", String.to_json f)
       ])

let of_json j =
  { id = Aws.Util.option_map (Aws.Json.lookup j "id") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; is_excludable = Aws.Util.option_map (Aws.Json.lookup j "isExcludable") Boolean.of_json
  }
