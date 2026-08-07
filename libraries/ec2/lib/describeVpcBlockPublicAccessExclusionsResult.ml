open Aws.BaseTypes

type t =
  { vpc_block_public_access_exclusions : VpcBlockPublicAccessExclusionList.t
  ; next_token : String.t option
  }

let make ?(vpc_block_public_access_exclusions = []) ?next_token () =
  { vpc_block_public_access_exclusions; next_token }

let parse xml =
  Some
    { vpc_block_public_access_exclusions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "vpcBlockPublicAccessExclusionSet" xml)
             VpcBlockPublicAccessExclusionList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VpcBlockPublicAccessExclusionSet"
              , VpcBlockPublicAccessExclusionList.to_query
                  v.vpc_block_public_access_exclusions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "vpcBlockPublicAccessExclusionSet"
           , VpcBlockPublicAccessExclusionList.to_json
               v.vpc_block_public_access_exclusions )
       ])

let of_json j =
  { vpc_block_public_access_exclusions =
      VpcBlockPublicAccessExclusionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcBlockPublicAccessExclusionSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
