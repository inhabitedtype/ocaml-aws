open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; attribute_names : AccountAttributeNameStringList.t
  }

let make ?dry_run ?(attribute_names = []) () = { dry_run; attribute_names }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; attribute_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "attributeName" xml)
             AccountAttributeNameStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AttributeName", AccountAttributeNameStringList.to_query v.attribute_names))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("attributeName", AccountAttributeNameStringList.to_json v.attribute_names)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; attribute_names =
      AccountAttributeNameStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "attributeName"))
  }
