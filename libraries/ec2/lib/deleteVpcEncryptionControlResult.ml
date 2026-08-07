type t = { vpc_encryption_control : VpcEncryptionControl.t option }

let make ?vpc_encryption_control () = { vpc_encryption_control }

let parse xml =
  Some
    { vpc_encryption_control =
        Aws.Util.option_bind
          (Aws.Xml.member "vpcEncryptionControl" xml)
          VpcEncryptionControl.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_encryption_control (fun f ->
             Aws.Query.Pair ("VpcEncryptionControl", VpcEncryptionControl.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_encryption_control (fun f ->
             "vpcEncryptionControl", VpcEncryptionControl.to_json f)
       ])

let of_json j =
  { vpc_encryption_control =
      Aws.Util.option_map
        (Aws.Json.lookup j "vpcEncryptionControl")
        VpcEncryptionControl.of_json
  }
