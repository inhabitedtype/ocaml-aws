open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; bucket : String.t
  ; prefix : String.t option
  }

let make ?dry_run ~bucket ?prefix () = { dry_run; bucket; prefix }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; bucket =
        Aws.Xml.required
          "bucket"
          (Aws.Util.option_bind (Aws.Xml.member "bucket" xml) String.parse)
    ; prefix = Aws.Util.option_bind (Aws.Xml.member "prefix" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.prefix (fun f ->
             Aws.Query.Pair ("Prefix", String.to_query f))
       ; Some (Aws.Query.Pair ("Bucket", String.to_query v.bucket))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.prefix (fun f -> "prefix", String.to_json f)
       ; Some ("bucket", String.to_json v.bucket)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; bucket = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "bucket"))
  ; prefix = Aws.Util.option_map (Aws.Json.lookup j "prefix") String.of_json
  }
