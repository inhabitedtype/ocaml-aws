open Aws.BaseTypes

type t =
  { image_block_public_access_state : ImageBlockPublicAccessEnabledState.t
  ; dry_run : Boolean.t option
  }

let make ~image_block_public_access_state ?dry_run () =
  { image_block_public_access_state; dry_run }

let parse xml =
  Some
    { image_block_public_access_state =
        Aws.Xml.required
          "ImageBlockPublicAccessState"
          (Aws.Util.option_bind
             (Aws.Xml.member "ImageBlockPublicAccessState" xml)
             ImageBlockPublicAccessEnabledState.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ImageBlockPublicAccessState"
              , ImageBlockPublicAccessEnabledState.to_query
                  v.image_block_public_access_state ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "ImageBlockPublicAccessState"
           , ImageBlockPublicAccessEnabledState.to_json v.image_block_public_access_state
           )
       ])

let of_json j =
  { image_block_public_access_state =
      ImageBlockPublicAccessEnabledState.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageBlockPublicAccessState"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
