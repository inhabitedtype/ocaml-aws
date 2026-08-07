open Aws.BaseTypes

type t =
  { resource_id_list : ResourceIdList.t
  ; next_token : String.t option
  }

let make ~resource_id_list ?next_token () = { resource_id_list; next_token }

let parse xml =
  Some
    { resource_id_list =
        Aws.Xml.required
          "ResourceIdList"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceIdList" xml)
             ResourceIdList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ResourceIdList.member", ResourceIdList.to_query v.resource_id_list))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("ResourceIdList", ResourceIdList.to_json v.resource_id_list)
       ])

let of_json j =
  { resource_id_list =
      ResourceIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIdList"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
