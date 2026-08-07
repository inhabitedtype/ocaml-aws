type t =
  { apple_internal : MacSystemIntegrityProtectionSettingStatus.t option
  ; base_system : MacSystemIntegrityProtectionSettingStatus.t option
  ; debugging_restrictions : MacSystemIntegrityProtectionSettingStatus.t option
  ; d_trace_restrictions : MacSystemIntegrityProtectionSettingStatus.t option
  ; filesystem_protections : MacSystemIntegrityProtectionSettingStatus.t option
  ; kext_signing : MacSystemIntegrityProtectionSettingStatus.t option
  ; nvram_protections : MacSystemIntegrityProtectionSettingStatus.t option
  ; status : MacSystemIntegrityProtectionSettingStatus.t option
  }

let make
    ?apple_internal
    ?base_system
    ?debugging_restrictions
    ?d_trace_restrictions
    ?filesystem_protections
    ?kext_signing
    ?nvram_protections
    ?status
    () =
  { apple_internal
  ; base_system
  ; debugging_restrictions
  ; d_trace_restrictions
  ; filesystem_protections
  ; kext_signing
  ; nvram_protections
  ; status
  }

let parse xml =
  Some
    { apple_internal =
        Aws.Util.option_bind
          (Aws.Xml.member "appleInternal" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; base_system =
        Aws.Util.option_bind
          (Aws.Xml.member "baseSystem" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; debugging_restrictions =
        Aws.Util.option_bind
          (Aws.Xml.member "debuggingRestrictions" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; d_trace_restrictions =
        Aws.Util.option_bind
          (Aws.Xml.member "dTraceRestrictions" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; filesystem_protections =
        Aws.Util.option_bind
          (Aws.Xml.member "filesystemProtections" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; kext_signing =
        Aws.Util.option_bind
          (Aws.Xml.member "kextSigning" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; nvram_protections =
        Aws.Util.option_bind
          (Aws.Xml.member "nvramProtections" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    ; status =
        Aws.Util.option_bind
          (Aws.Xml.member "status" xml)
          MacSystemIntegrityProtectionSettingStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair
               ("Status", MacSystemIntegrityProtectionSettingStatus.to_query f))
       ; Aws.Util.option_map v.nvram_protections (fun f ->
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
       [ Aws.Util.option_map v.status (fun f ->
             "status", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.nvram_protections (fun f ->
             "nvramProtections", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.kext_signing (fun f ->
             "kextSigning", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.filesystem_protections (fun f ->
             "filesystemProtections", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.d_trace_restrictions (fun f ->
             "dTraceRestrictions", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.debugging_restrictions (fun f ->
             "debuggingRestrictions", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.base_system (fun f ->
             "baseSystem", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ; Aws.Util.option_map v.apple_internal (fun f ->
             "appleInternal", MacSystemIntegrityProtectionSettingStatus.to_json f)
       ])

let of_json j =
  { apple_internal =
      Aws.Util.option_map
        (Aws.Json.lookup j "appleInternal")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; base_system =
      Aws.Util.option_map
        (Aws.Json.lookup j "baseSystem")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; debugging_restrictions =
      Aws.Util.option_map
        (Aws.Json.lookup j "debuggingRestrictions")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; d_trace_restrictions =
      Aws.Util.option_map
        (Aws.Json.lookup j "dTraceRestrictions")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; filesystem_protections =
      Aws.Util.option_map
        (Aws.Json.lookup j "filesystemProtections")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; kext_signing =
      Aws.Util.option_map
        (Aws.Json.lookup j "kextSigning")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; nvram_protections =
      Aws.Util.option_map
        (Aws.Json.lookup j "nvramProtections")
        MacSystemIntegrityProtectionSettingStatus.of_json
  ; status =
      Aws.Util.option_map
        (Aws.Json.lookup j "status")
        MacSystemIntegrityProtectionSettingStatus.of_json
  }
