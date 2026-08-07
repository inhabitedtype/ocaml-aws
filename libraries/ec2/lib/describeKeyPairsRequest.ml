open Aws.BaseTypes

type t =
  { key_names : KeyNameStringList.t
  ; key_pair_ids : KeyPairIdStringList.t
  ; include_public_key : Boolean.t option
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  }

let make
    ?(key_names = [])
    ?(key_pair_ids = [])
    ?include_public_key
    ?dry_run
    ?(filters = [])
    () =
  { key_names; key_pair_ids; include_public_key; dry_run; filters }

let parse xml =
  Some
    { key_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "KeyName" xml) KeyNameStringList.parse)
    ; key_pair_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "KeyPairId" xml)
             KeyPairIdStringList.parse)
    ; include_public_key =
        Aws.Util.option_bind (Aws.Xml.member "IncludePublicKey" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.include_public_key (fun f ->
             Aws.Query.Pair ("IncludePublicKey", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("KeyPairId", KeyPairIdStringList.to_query v.key_pair_ids))
       ; Some (Aws.Query.Pair ("KeyName", KeyNameStringList.to_query v.key_names))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.include_public_key (fun f ->
             "IncludePublicKey", Boolean.to_json f)
       ; Some ("KeyPairId", KeyPairIdStringList.to_json v.key_pair_ids)
       ; Some ("KeyName", KeyNameStringList.to_json v.key_names)
       ])

let of_json j =
  { key_names =
      KeyNameStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyName"))
  ; key_pair_ids =
      KeyPairIdStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyPairId"))
  ; include_public_key =
      Aws.Util.option_map (Aws.Json.lookup j "IncludePublicKey") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
