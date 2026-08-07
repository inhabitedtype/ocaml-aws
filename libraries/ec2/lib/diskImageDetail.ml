open Aws.BaseTypes

type t =
  { format : DiskImageFormat.t
  ; bytes : Long.t
  ; import_manifest_url : String.t
  }

let make ~format ~bytes ~import_manifest_url () = { format; bytes; import_manifest_url }

let parse xml =
  Some
    { format =
        Aws.Xml.required
          "format"
          (Aws.Util.option_bind (Aws.Xml.member "format" xml) DiskImageFormat.parse)
    ; bytes =
        Aws.Xml.required
          "bytes"
          (Aws.Util.option_bind (Aws.Xml.member "bytes" xml) Long.parse)
    ; import_manifest_url =
        Aws.Xml.required
          "importManifestUrl"
          (Aws.Util.option_bind (Aws.Xml.member "importManifestUrl" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ImportManifestUrl", String.to_query v.import_manifest_url))
       ; Some (Aws.Query.Pair ("Bytes", Long.to_query v.bytes))
       ; Some (Aws.Query.Pair ("Format", DiskImageFormat.to_query v.format))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("importManifestUrl", String.to_json v.import_manifest_url)
       ; Some ("bytes", Long.to_json v.bytes)
       ; Some ("format", DiskImageFormat.to_json v.format)
       ])

let of_json j =
  { format = DiskImageFormat.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "format"))
  ; bytes = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "bytes"))
  ; import_manifest_url =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "importManifestUrl"))
  }
