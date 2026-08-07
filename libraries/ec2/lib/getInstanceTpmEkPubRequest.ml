open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; key_type : EkPubKeyType.t
  ; key_format : EkPubKeyFormat.t
  ; dry_run : Boolean.t option
  }

let make ~instance_id ~key_type ~key_format ?dry_run () =
  { instance_id; key_type; key_format; dry_run }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; key_type =
        Aws.Xml.required
          "KeyType"
          (Aws.Util.option_bind (Aws.Xml.member "KeyType" xml) EkPubKeyType.parse)
    ; key_format =
        Aws.Xml.required
          "KeyFormat"
          (Aws.Util.option_bind (Aws.Xml.member "KeyFormat" xml) EkPubKeyFormat.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("KeyFormat", EkPubKeyFormat.to_query v.key_format))
       ; Some (Aws.Query.Pair ("KeyType", EkPubKeyType.to_query v.key_type))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("KeyFormat", EkPubKeyFormat.to_json v.key_format)
       ; Some ("KeyType", EkPubKeyType.to_json v.key_type)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; key_type = EkPubKeyType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyType"))
  ; key_format =
      EkPubKeyFormat.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyFormat"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
