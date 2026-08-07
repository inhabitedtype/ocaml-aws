open Aws.BaseTypes

type t =
  { public_key_list : PublicKeyList.t
  ; next_token : String.t option
  }

let make ?(public_key_list = []) ?next_token () = { public_key_list; next_token }

let parse xml =
  Some
    { public_key_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "PublicKeyList" xml) PublicKeyList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("PublicKeyList.member", PublicKeyList.to_query v.public_key_list))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("PublicKeyList", PublicKeyList.to_json v.public_key_list)
       ])

let of_json j =
  { public_key_list =
      PublicKeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PublicKeyList"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
