open Aws.BaseTypes

type t =
  { device_id : String.t option
  ; vendor_id : String.t option
  ; subsystem_id : String.t option
  ; subsystem_vendor_id : String.t option
  }

let make ?device_id ?vendor_id ?subsystem_id ?subsystem_vendor_id () =
  { device_id; vendor_id; subsystem_id; subsystem_vendor_id }

let parse xml =
  Some
    { device_id = Aws.Util.option_bind (Aws.Xml.member "DeviceId" xml) String.parse
    ; vendor_id = Aws.Util.option_bind (Aws.Xml.member "VendorId" xml) String.parse
    ; subsystem_id = Aws.Util.option_bind (Aws.Xml.member "SubsystemId" xml) String.parse
    ; subsystem_vendor_id =
        Aws.Util.option_bind (Aws.Xml.member "SubsystemVendorId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subsystem_vendor_id (fun f ->
             Aws.Query.Pair ("SubsystemVendorId", String.to_query f))
       ; Aws.Util.option_map v.subsystem_id (fun f ->
             Aws.Query.Pair ("SubsystemId", String.to_query f))
       ; Aws.Util.option_map v.vendor_id (fun f ->
             Aws.Query.Pair ("VendorId", String.to_query f))
       ; Aws.Util.option_map v.device_id (fun f ->
             Aws.Query.Pair ("DeviceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subsystem_vendor_id (fun f ->
             "SubsystemVendorId", String.to_json f)
       ; Aws.Util.option_map v.subsystem_id (fun f -> "SubsystemId", String.to_json f)
       ; Aws.Util.option_map v.vendor_id (fun f -> "VendorId", String.to_json f)
       ; Aws.Util.option_map v.device_id (fun f -> "DeviceId", String.to_json f)
       ])

let of_json j =
  { device_id = Aws.Util.option_map (Aws.Json.lookup j "DeviceId") String.of_json
  ; vendor_id = Aws.Util.option_map (Aws.Json.lookup j "VendorId") String.of_json
  ; subsystem_id = Aws.Util.option_map (Aws.Json.lookup j "SubsystemId") String.of_json
  ; subsystem_vendor_id =
      Aws.Util.option_map (Aws.Json.lookup j "SubsystemVendorId") String.of_json
  }
