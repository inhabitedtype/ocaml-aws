open Aws.BaseTypes

type t =
  { vpc_encryption_controls : VpcEncryptionControlList.t
  ; next_token : String.t option
  }

let make ?(vpc_encryption_controls = []) ?next_token () =
  { vpc_encryption_controls; next_token }

let parse xml =
  Some
    { vpc_encryption_controls =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "vpcEncryptionControlSet" xml)
             VpcEncryptionControlList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VpcEncryptionControlSet"
              , VpcEncryptionControlList.to_query v.vpc_encryption_controls ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "vpcEncryptionControlSet"
           , VpcEncryptionControlList.to_json v.vpc_encryption_controls )
       ])

let of_json j =
  { vpc_encryption_controls =
      VpcEncryptionControlList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcEncryptionControlSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
