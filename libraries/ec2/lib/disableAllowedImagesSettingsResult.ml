type t = { allowed_images_settings_state : AllowedImagesSettingsDisabledState.t option }

let make ?allowed_images_settings_state () = { allowed_images_settings_state }

let parse xml =
  Some
    { allowed_images_settings_state =
        Aws.Util.option_bind
          (Aws.Xml.member "allowedImagesSettingsState" xml)
          AllowedImagesSettingsDisabledState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allowed_images_settings_state (fun f ->
             Aws.Query.Pair
               ( "AllowedImagesSettingsState"
               , AllowedImagesSettingsDisabledState.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allowed_images_settings_state (fun f ->
             "allowedImagesSettingsState", AllowedImagesSettingsDisabledState.to_json f)
       ])

let of_json j =
  { allowed_images_settings_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "allowedImagesSettingsState")
        AllowedImagesSettingsDisabledState.of_json
  }
