open Aws.BaseTypes

type t =
  { key_name : String.t
  ; key_type : KeyType.t option
  ; tag_specifications : TagSpecificationList.t
  ; key_format : KeyFormat.t option
  ; dry_run : Boolean.t option
  }

let make ~key_name ?key_type ?(tag_specifications = []) ?key_format ?dry_run () =
  { key_name; key_type; tag_specifications; key_format; dry_run }

let parse xml =
  Some
    { key_name =
        Aws.Xml.required
          "KeyName"
          (Aws.Util.option_bind (Aws.Xml.member "KeyName" xml) String.parse)
    ; key_type = Aws.Util.option_bind (Aws.Xml.member "KeyType" xml) KeyType.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; key_format = Aws.Util.option_bind (Aws.Xml.member "KeyFormat" xml) KeyFormat.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.key_format (fun f ->
             Aws.Query.Pair ("KeyFormat", KeyFormat.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.key_type (fun f ->
             Aws.Query.Pair ("KeyType", KeyType.to_query f))
       ; Some (Aws.Query.Pair ("KeyName", String.to_query v.key_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.key_format (fun f -> "KeyFormat", KeyFormat.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.key_type (fun f -> "KeyType", KeyType.to_json f)
       ; Some ("KeyName", String.to_json v.key_name)
       ])

let of_json j =
  { key_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyName"))
  ; key_type = Aws.Util.option_map (Aws.Json.lookup j "KeyType") KeyType.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; key_format = Aws.Util.option_map (Aws.Json.lookup j "KeyFormat") KeyFormat.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
