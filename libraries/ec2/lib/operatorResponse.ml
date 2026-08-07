open Aws.BaseTypes

type t =
  { managed : Boolean.t option
  ; principal : String.t option
  ; hidden_by_default : Boolean.t option
  }

let make ?managed ?principal ?hidden_by_default () =
  { managed; principal; hidden_by_default }

let parse xml =
  Some
    { managed = Aws.Util.option_bind (Aws.Xml.member "managed" xml) Boolean.parse
    ; principal = Aws.Util.option_bind (Aws.Xml.member "principal" xml) String.parse
    ; hidden_by_default =
        Aws.Util.option_bind (Aws.Xml.member "hiddenByDefault" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hidden_by_default (fun f ->
             Aws.Query.Pair ("HiddenByDefault", Boolean.to_query f))
       ; Aws.Util.option_map v.principal (fun f ->
             Aws.Query.Pair ("Principal", String.to_query f))
       ; Aws.Util.option_map v.managed (fun f ->
             Aws.Query.Pair ("Managed", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hidden_by_default (fun f ->
             "hiddenByDefault", Boolean.to_json f)
       ; Aws.Util.option_map v.principal (fun f -> "principal", String.to_json f)
       ; Aws.Util.option_map v.managed (fun f -> "managed", Boolean.to_json f)
       ])

let of_json j =
  { managed = Aws.Util.option_map (Aws.Json.lookup j "managed") Boolean.of_json
  ; principal = Aws.Util.option_map (Aws.Json.lookup j "principal") String.of_json
  ; hidden_by_default =
      Aws.Util.option_map (Aws.Json.lookup j "hiddenByDefault") Boolean.of_json
  }
