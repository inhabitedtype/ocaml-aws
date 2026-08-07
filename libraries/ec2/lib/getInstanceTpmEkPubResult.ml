open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; key_type : EkPubKeyType.t option
  ; key_format : EkPubKeyFormat.t option
  ; key_value : String.t option
  }

let make ?instance_id ?key_type ?key_format ?key_value () =
  { instance_id; key_type; key_format; key_value }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; key_type = Aws.Util.option_bind (Aws.Xml.member "keyType" xml) EkPubKeyType.parse
    ; key_format =
        Aws.Util.option_bind (Aws.Xml.member "keyFormat" xml) EkPubKeyFormat.parse
    ; key_value = Aws.Util.option_bind (Aws.Xml.member "keyValue" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_value (fun f ->
             Aws.Query.Pair ("KeyValue", String.to_query f))
       ; Aws.Util.option_map v.key_format (fun f ->
             Aws.Query.Pair ("KeyFormat", EkPubKeyFormat.to_query f))
       ; Aws.Util.option_map v.key_type (fun f ->
             Aws.Query.Pair ("KeyType", EkPubKeyType.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_value (fun f -> "keyValue", String.to_json f)
       ; Aws.Util.option_map v.key_format (fun f -> "keyFormat", EkPubKeyFormat.to_json f)
       ; Aws.Util.option_map v.key_type (fun f -> "keyType", EkPubKeyType.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; key_type = Aws.Util.option_map (Aws.Json.lookup j "keyType") EkPubKeyType.of_json
  ; key_format =
      Aws.Util.option_map (Aws.Json.lookup j "keyFormat") EkPubKeyFormat.of_json
  ; key_value = Aws.Util.option_map (Aws.Json.lookup j "keyValue") String.of_json
  }
