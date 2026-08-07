open Aws.BaseTypes

type t =
  { allowed_images_settings_state : AllowedImagesSettingsEnabledState.t
  ; dry_run : Boolean.t option
  }

let make ~allowed_images_settings_state ?dry_run () =
  { allowed_images_settings_state; dry_run }

let parse xml =
  Some
    { allowed_images_settings_state =
        Aws.Xml.required
          "AllowedImagesSettingsState"
          (Aws.Util.option_bind
             (Aws.Xml.member "AllowedImagesSettingsState" xml)
             AllowedImagesSettingsEnabledState.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AllowedImagesSettingsState"
              , AllowedImagesSettingsEnabledState.to_query v.allowed_images_settings_state
              ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "AllowedImagesSettingsState"
           , AllowedImagesSettingsEnabledState.to_json v.allowed_images_settings_state )
       ])

let of_json j =
  { allowed_images_settings_state =
      AllowedImagesSettingsEnabledState.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AllowedImagesSettingsState"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
