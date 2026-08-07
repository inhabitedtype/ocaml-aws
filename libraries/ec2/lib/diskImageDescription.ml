open Aws.BaseTypes

type t =
  { checksum : String.t option
  ; format : DiskImageFormat.t option
  ; import_manifest_url : String.t option
  ; size : Long.t option
  }

let make ?checksum ?format ?import_manifest_url ?size () =
  { checksum; format; import_manifest_url; size }

let parse xml =
  Some
    { checksum = Aws.Util.option_bind (Aws.Xml.member "checksum" xml) String.parse
    ; format = Aws.Util.option_bind (Aws.Xml.member "format" xml) DiskImageFormat.parse
    ; import_manifest_url =
        Aws.Util.option_bind (Aws.Xml.member "importManifestUrl" xml) String.parse
    ; size = Aws.Util.option_bind (Aws.Xml.member "size" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.size (fun f -> Aws.Query.Pair ("Size", Long.to_query f))
       ; Aws.Util.option_map v.import_manifest_url (fun f ->
             Aws.Query.Pair ("ImportManifestUrl", String.to_query f))
       ; Aws.Util.option_map v.format (fun f ->
             Aws.Query.Pair ("Format", DiskImageFormat.to_query f))
       ; Aws.Util.option_map v.checksum (fun f ->
             Aws.Query.Pair ("Checksum", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.size (fun f -> "size", Long.to_json f)
       ; Aws.Util.option_map v.import_manifest_url (fun f ->
             "importManifestUrl", String.to_json f)
       ; Aws.Util.option_map v.format (fun f -> "format", DiskImageFormat.to_json f)
       ; Aws.Util.option_map v.checksum (fun f -> "checksum", String.to_json f)
       ])

let of_json j =
  { checksum = Aws.Util.option_map (Aws.Json.lookup j "checksum") String.of_json
  ; format = Aws.Util.option_map (Aws.Json.lookup j "format") DiskImageFormat.of_json
  ; import_manifest_url =
      Aws.Util.option_map (Aws.Json.lookup j "importManifestUrl") String.of_json
  ; size = Aws.Util.option_map (Aws.Json.lookup j "size") Long.of_json
  }
