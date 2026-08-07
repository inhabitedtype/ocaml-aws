open Aws.BaseTypes

type t =
  { key_name : String.t option
  ; key_pair_id : String.t option
  ; dry_run : Boolean.t option
  }

let make ?key_name ?key_pair_id ?dry_run () = { key_name; key_pair_id; dry_run }

let parse xml =
  Some
    { key_name = Aws.Util.option_bind (Aws.Xml.member "KeyName" xml) String.parse
    ; key_pair_id = Aws.Util.option_bind (Aws.Xml.member "KeyPairId" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.key_pair_id (fun f ->
             Aws.Query.Pair ("KeyPairId", String.to_query f))
       ; Aws.Util.option_map v.key_name (fun f ->
             Aws.Query.Pair ("KeyName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.key_pair_id (fun f -> "KeyPairId", String.to_json f)
       ; Aws.Util.option_map v.key_name (fun f -> "KeyName", String.to_json f)
       ])

let of_json j =
  { key_name = Aws.Util.option_map (Aws.Json.lookup j "KeyName") String.of_json
  ; key_pair_id = Aws.Util.option_map (Aws.Json.lookup j "KeyPairId") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
