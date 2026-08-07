open Aws.BaseTypes

type t =
  | Description
  | Kernel
  | Ramdisk
  | LaunchPermission
  | ProductCodes
  | BlockDeviceMapping
  | SriovNetSupport
  | BootMode
  | TpmSupport
  | UefiData
  | LastLaunchedTime
  | ImdsSupport
  | DeregistrationProtection

let str_to_t =
  [ "deregistrationProtection", DeregistrationProtection
  ; "imdsSupport", ImdsSupport
  ; "lastLaunchedTime", LastLaunchedTime
  ; "uefiData", UefiData
  ; "tpmSupport", TpmSupport
  ; "bootMode", BootMode
  ; "sriovNetSupport", SriovNetSupport
  ; "blockDeviceMapping", BlockDeviceMapping
  ; "productCodes", ProductCodes
  ; "launchPermission", LaunchPermission
  ; "ramdisk", Ramdisk
  ; "kernel", Kernel
  ; "description", Description
  ]

let t_to_str =
  [ DeregistrationProtection, "deregistrationProtection"
  ; ImdsSupport, "imdsSupport"
  ; LastLaunchedTime, "lastLaunchedTime"
  ; UefiData, "uefiData"
  ; TpmSupport, "tpmSupport"
  ; BootMode, "bootMode"
  ; SriovNetSupport, "sriovNetSupport"
  ; BlockDeviceMapping, "blockDeviceMapping"
  ; ProductCodes, "productCodes"
  ; LaunchPermission, "launchPermission"
  ; Ramdisk, "ramdisk"
  ; Kernel, "kernel"
  ; Description, "description"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
