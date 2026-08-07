open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; prefix_list_id : String.t
  ; previous_version : Long.t
  ; current_version : Long.t
  }

let make ?dry_run ~prefix_list_id ~previous_version ~current_version () =
  { dry_run; prefix_list_id; previous_version; current_version }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; prefix_list_id =
        Aws.Xml.required
          "PrefixListId"
          (Aws.Util.option_bind (Aws.Xml.member "PrefixListId" xml) String.parse)
    ; previous_version =
        Aws.Xml.required
          "PreviousVersion"
          (Aws.Util.option_bind (Aws.Xml.member "PreviousVersion" xml) Long.parse)
    ; current_version =
        Aws.Xml.required
          "CurrentVersion"
          (Aws.Util.option_bind (Aws.Xml.member "CurrentVersion" xml) Long.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("CurrentVersion", Long.to_query v.current_version))
       ; Some (Aws.Query.Pair ("PreviousVersion", Long.to_query v.previous_version))
       ; Some (Aws.Query.Pair ("PrefixListId", String.to_query v.prefix_list_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CurrentVersion", Long.to_json v.current_version)
       ; Some ("PreviousVersion", Long.to_json v.previous_version)
       ; Some ("PrefixListId", String.to_json v.prefix_list_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; prefix_list_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrefixListId"))
  ; previous_version =
      Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PreviousVersion"))
  ; current_version =
      Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CurrentVersion"))
  }
