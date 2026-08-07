open Aws.BaseTypes

type t =
  { activate_tag_keys : ValueStringList.t
  ; deactivate_tag_keys : ValueStringList.t
  ; dry_run : Boolean.t option
  ; client_token : String.t option
  }

let make ?(activate_tag_keys = []) ?(deactivate_tag_keys = []) ?dry_run ?client_token () =
  { activate_tag_keys; deactivate_tag_keys; dry_run; client_token }

let parse xml =
  Some
    { activate_tag_keys =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ActivateTagKey" xml)
             ValueStringList.parse)
    ; deactivate_tag_keys =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DeactivateTagKey" xml)
             ValueStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DeactivateTagKey", ValueStringList.to_query v.deactivate_tag_keys))
       ; Some
           (Aws.Query.Pair ("ActivateTagKey", ValueStringList.to_query v.activate_tag_keys))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("DeactivateTagKey", ValueStringList.to_json v.deactivate_tag_keys)
       ; Some ("ActivateTagKey", ValueStringList.to_json v.activate_tag_keys)
       ])

let of_json j =
  { activate_tag_keys =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ActivateTagKey"))
  ; deactivate_tag_keys =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DeactivateTagKey"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  }
