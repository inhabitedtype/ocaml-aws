open Aws.BaseTypes

type t =
  { version : String.t option
  ; is_default : Boolean.t option
  }

let make ?version ?is_default () = { version; is_default }

let parse xml =
  Some
    { version = Aws.Util.option_bind (Aws.Xml.member "Version" xml) String.parse
    ; is_default = Aws.Util.option_bind (Aws.Xml.member "IsDefault" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_default (fun f ->
             Aws.Query.Pair ("IsDefault", Boolean.to_query f))
       ; Aws.Util.option_map v.version (fun f ->
             Aws.Query.Pair ("Version", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_default (fun f -> "IsDefault", Boolean.to_json f)
       ; Aws.Util.option_map v.version (fun f -> "Version", String.to_json f)
       ])

let of_json j =
  { version = Aws.Util.option_map (Aws.Json.lookup j "Version") String.of_json
  ; is_default = Aws.Util.option_map (Aws.Json.lookup j "IsDefault") Boolean.of_json
  }
