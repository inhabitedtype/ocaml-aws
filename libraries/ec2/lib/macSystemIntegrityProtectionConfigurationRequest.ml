type t =
  { apple_internal : MacSystemIntegrityProtectionSettingStatus.t option
  ; base_system : MacSystemIntegrityProtectionSettingStatus.t option
  ; debugging_restrictions : MacSystemIntegrityProtectionSettingStatus.t option
  ; d_trace_restrictions : MacSystemIntegrityProtectionSettingStatus.t option
  ; filesystem_protections : MacSystemIntegrityProtectionSettingStatus.t option
  ; kext_signing : MacSystemIntegrityProtectionSettingStatus.t option
  ; nvram_protections : MacSystemIntegrityProtectionSettingStatus.t option
  }

let make
    ?apple_internal
    ?base_system
    ?debugging_restrictions
    ?d_trace_restrictions
    ?filesystem_protections
    ?kext_signing
    ?nvram_protections
    () =
  { apple_internal
  ; base_system
  ; debugging_restrictions
  ; d_trace_restrictions
  ; filesystem_protections
  ; kext_signing
  ; nvram_protections
  }

let parse xml =
  Some
    { apple_internal =
        Aws.Util.option_bind
          (Aws.Xml.member "AppleInternal" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; base_system =
        Aws.Util.option_bind
          (Aws.Xml.member "BaseSystem" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; debugging_restrictions =
        Aws.Util.option_bind
          (Aws.Xml.member "DebuggingRestrictions" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; d_trace_restrictions =
        Aws.Util.option_bind
          (Aws.Xml.member "DTraceRestrictions" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; filesystem_protections =
        Aws.Util.option_bind
          (Aws.Xml.member "FilesystemProtections" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; kext_signing =
        Aws.Util.option_bind
          (Aws.Xml.member "KextSigning" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; nvram_protections =
        Aws.Util.option_bind
          (Aws.Xml.member "NvramProtections" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nvram_protections (fun f ->
             Aws.Query.Pair
               ("NvramProtections", MacSystemIntegrityProtectionSettingStatus.to_query f))
       ; Aws.Util.option_map v.kext_signing (fun f ->
             Aws.Query.Pair
               ("KextSigning", MacSystemIntegrityProtectionSettingStatus.to_query f))
       ; Aws.Util.option_map v.filesystem_protections (fun f ->
             Aws.Query.Pair
               ( "FilesystemProtections"
               , MacSystemIntegrityProtectionSettingStatus.to_query f ))
       ; Aws.Util.option_map v.d_trace_restrictions (fun f ->
             Aws.Query.Pair
               ("DTraceRestrictions", MacSystemIntegrityProtectionSettingStatus.to_query f))
       ; Aws.Util.option_map v.debugging_restrictions (fun f ->
             Aws.Query.Pair
               ( "DebuggingRestrictions"
               , MacSystemIntegrityProtectionSettingStatus.to_query f ))
       ; Aws.Util.option_map v.base_system (fun f ->
             Aws.Query.Pair
               ("BaseSystem", MacSystemIntegrityProtectionSettingStatus.to_query f))
       ; Aws.Util.option_map v.apple_internal (fun f ->
             Aws.Query.Pair
               ("AppleInternal", MacSystemIntegrityProtectionSettingStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nvram_protections (fun f ->
             "NvramProtections", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.kext_signing (fun f ->
             "KextSigning", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.filesystem_protections (fun f ->
             "FilesystemProtections", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.d_trace_restrictions (fun f ->
             "DTraceRestrictions", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.debugging_restrictions (fun f ->
             "DebuggingRestrictions", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.base_system (fun f ->
             "BaseSystem", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.apple_internal (fun f ->
             "AppleInternal", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ])

let of_json j =
  { apple_internal =
      Aws.Util.option_map
        (Aws.Json.lookup j "AppleInternal")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; base_system =
      Aws.Util.option_map
        (Aws.Json.lookup j "BaseSystem")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; debugging_restrictions =
      Aws.Util.option_map
        (Aws.Json.lookup j "DebuggingRestrictions")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; d_trace_restrictions =
      Aws.Util.option_map
        (Aws.Json.lookup j "DTraceRestrictions")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; filesystem_protections =
      Aws.Util.option_map
        (Aws.Json.lookup j "FilesystemProtections")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; kext_signing =
      Aws.Util.option_map
        (Aws.Json.lookup j "KextSigning")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; nvram_protections =
      Aws.Util.option_map
        (Aws.Json.lookup j "NvramProtections")
        MacSystemIntegrityProtectionSettingStatus.of_json
  }
