open Aws.BaseTypes

type t =
  { tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  ; key_name : String.t
  ; public_key_material : Blob.t
  }

let make ?(tag_specifications = []) ?dry_run ~key_name ~public_key_material () =
  { tag_specifications; dry_run; key_name; public_key_material }

let parse xml =
  Some
    { tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; key_name =
        Aws.Xml.required
          "keyName"
          (Aws.Util.option_bind (Aws.Xml.member "keyName" xml) String.parse)
    ; public_key_material =
        Aws.Xml.required
          "publicKeyMaterial"
          (Aws.Util.option_bind (Aws.Xml.member "publicKeyMaterial" xml) Blob.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PublicKeyMaterial", Blob.to_query v.public_key_material))
       ; Some (Aws.Query.Pair ("KeyName", String.to_query v.key_name))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("publicKeyMaterial", Blob.to_json v.public_key_material)
       ; Some ("keyName", String.to_json v.key_name)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ])

let of_json j =
  { tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; key_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "keyName"))
  ; public_key_material =
      Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "publicKeyMaterial"))
  }
