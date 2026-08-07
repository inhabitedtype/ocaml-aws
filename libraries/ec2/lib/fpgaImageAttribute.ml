open Aws.BaseTypes

type t =
  { fpga_image_id : String.t option
  ; name : String.t option
  ; description : String.t option
  ; load_permissions : LoadPermissionList.t
  ; product_codes : ProductCodeList.t
  }

let make
    ?fpga_image_id
    ?name
    ?description
    ?(load_permissions = [])
    ?(product_codes = [])
    () =
  { fpga_image_id; name; description; load_permissions; product_codes }

let parse xml =
  Some
    { fpga_image_id = Aws.Util.option_bind (Aws.Xml.member "fpgaImageId" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; load_permissions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "loadPermissions" xml)
             LoadPermissionList.parse)
    ; product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "productCodes" xml) ProductCodeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ProductCodes", ProductCodeList.to_query v.product_codes))
       ; Some
           (Aws.Query.Pair
              ("LoadPermissions", LoadPermissionList.to_query v.load_permissions))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.fpga_image_id (fun f ->
             Aws.Query.Pair ("FpgaImageId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("productCodes", ProductCodeList.to_json v.product_codes)
       ; Some ("loadPermissions", LoadPermissionList.to_json v.load_permissions)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "name", String.to_json f)
       ; Aws.Util.option_map v.fpga_image_id (fun f -> "fpgaImageId", String.to_json f)
       ])

let of_json j =
  { fpga_image_id = Aws.Util.option_map (Aws.Json.lookup j "fpgaImageId") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; load_permissions =
      LoadPermissionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "loadPermissions"))
  ; product_codes =
      ProductCodeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "productCodes"))
  }
