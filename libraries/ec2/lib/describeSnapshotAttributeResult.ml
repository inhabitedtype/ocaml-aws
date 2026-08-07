open Aws.BaseTypes

type t =
  { product_codes : ProductCodeList.t
  ; snapshot_id : String.t option
  ; create_volume_permissions : CreateVolumePermissionList.t
  }

let make ?(product_codes = []) ?snapshot_id ?(create_volume_permissions = []) () =
  { product_codes; snapshot_id; create_volume_permissions }

let parse xml =
  Some
    { product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "productCodes" xml) ProductCodeList.parse)
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; create_volume_permissions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "createVolumePermission" xml)
             CreateVolumePermissionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CreateVolumePermission"
              , CreateVolumePermissionList.to_query v.create_volume_permissions ))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Some (Aws.Query.Pair ("ProductCodes", ProductCodeList.to_query v.product_codes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "createVolumePermission"
           , CreateVolumePermissionList.to_json v.create_volume_permissions )
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ; Some ("productCodes", ProductCodeList.to_json v.product_codes)
       ])

let of_json j =
  { product_codes =
      ProductCodeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "productCodes"))
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; create_volume_permissions =
      CreateVolumePermissionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "createVolumePermission"))
  }
