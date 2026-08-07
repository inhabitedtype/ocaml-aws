type t = { allowed_images_settings_state : AllowedImagesSettingsEnabledState.t option }

let make ?allowed_images_settings_state () = { allowed_images_settings_state }

let parse xml =
  Some
    { allowed_images_settings_state =
        Aws.Util.option_bind
          (Aws.Xml.member "allowedImagesSettingsState" xml)
          AllowedImagesSettingsEnabledState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allowed_images_settings_state (fun f ->
             Aws.Query.Pair
               ("AllowedImagesSettingsState", AllowedImagesSettingsEnabledState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allowed_images_settings_state (fun f ->
             "allowedImagesSettingsState", AllowedImagesSettingsEnabledState.to_json f)
       ])

let of_json j =
  { allowed_images_settings_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "allowedImagesSettingsState")
        AllowedImagesSettingsEnabledState.of_json
  }
