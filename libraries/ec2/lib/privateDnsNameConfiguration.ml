open Aws.BaseTypes

type t =
  { state : DnsNameState.t option
  ; type_ : String.t option
  ; value : String.t option
  ; name : String.t option
  }

let make ?state ?type_ ?value ?name () = { state; type_; value; name }

let parse xml =
  Some
    { state = Aws.Util.option_bind (Aws.Xml.member "state" xml) DnsNameState.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) String.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "value" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f -> Aws.Query.Pair ("Type", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", DnsNameState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> "name", String.to_json f)
       ; Aws.Util.option_map v.value (fun f -> "value", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", DnsNameState.to_json f)
       ])

let of_json j =
  { state = Aws.Util.option_map (Aws.Json.lookup j "state") DnsNameState.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") String.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "value") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  }
