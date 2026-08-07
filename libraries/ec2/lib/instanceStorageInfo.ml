open Aws.BaseTypes

type t =
  { total_size_in_g_b : Long.t option
  ; disks : DiskInfoList.t
  ; nvme_support : EphemeralNvmeSupport.t option
  ; encryption_support : InstanceStorageEncryptionSupport.t option
  }

let make ?total_size_in_g_b ?(disks = []) ?nvme_support ?encryption_support () =
  { total_size_in_g_b; disks; nvme_support; encryption_support }

let parse xml =
  Some
    { total_size_in_g_b =
        Aws.Util.option_bind (Aws.Xml.member "totalSizeInGB" xml) Long.parse
    ; disks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "disks" xml) DiskInfoList.parse)
    ; nvme_support =
        Aws.Util.option_bind (Aws.Xml.member "nvmeSupport" xml) EphemeralNvmeSupport.parse
    ; encryption_support =
        Aws.Util.option_bind
          (Aws.Xml.member "encryptionSupport" xml)
          InstanceStorageEncryptionSupport.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.encryption_support (fun f ->
             Aws.Query.Pair
               ("EncryptionSupport", InstanceStorageEncryptionSupport.to_query f))
       ; Aws.Util.option_map v.nvme_support (fun f ->
             Aws.Query.Pair ("NvmeSupport", EphemeralNvmeSupport.to_query f))
       ; Some (Aws.Query.Pair ("Disks", DiskInfoList.to_query v.disks))
       ; Aws.Util.option_map v.total_size_in_g_b (fun f ->
             Aws.Query.Pair ("TotalSizeInGB", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.encryption_support (fun f ->
             "encryptionSupport", InstanceStorageEncryptionSupport.to_json f)
       ; Aws.Util.option_map v.nvme_support (fun f ->
             "nvmeSupport", EphemeralNvmeSupport.to_json f)
       ; Some ("disks", DiskInfoList.to_json v.disks)
       ; Aws.Util.option_map v.total_size_in_g_b (fun f ->
             "totalSizeInGB", Long.to_json f)
       ])

let of_json j =
  { total_size_in_g_b =
      Aws.Util.option_map (Aws.Json.lookup j "totalSizeInGB") Long.of_json
  ; disks = DiskInfoList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "disks"))
  ; nvme_support =
      Aws.Util.option_map (Aws.Json.lookup j "nvmeSupport") EphemeralNvmeSupport.of_json
  ; encryption_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "encryptionSupport")
        InstanceStorageEncryptionSupport.of_json
  }
