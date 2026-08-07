open Aws.BaseTypes

type t =
  { non_compliant_resources : VpcEncryptionNonCompliantResourceList.t
  ; next_token : String.t option
  }

let make ?(non_compliant_resources = []) ?next_token () =
  { non_compliant_resources; next_token }

let parse xml =
  Some
    { non_compliant_resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "nonCompliantResourceSet" xml)
             VpcEncryptionNonCompliantResourceList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NonCompliantResourceSet"
              , VpcEncryptionNonCompliantResourceList.to_query v.non_compliant_resources
              ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "nonCompliantResourceSet"
           , VpcEncryptionNonCompliantResourceList.to_json v.non_compliant_resources )
       ])

let of_json j =
  { non_compliant_resources =
      VpcEncryptionNonCompliantResourceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "nonCompliantResourceSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
