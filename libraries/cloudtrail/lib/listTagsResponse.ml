open Aws.BaseTypes

type t =
  { resource_tag_list : ResourceTagList.t
  ; next_token : String.t option
  }

let make ?(resource_tag_list = []) ?next_token () = { resource_tag_list; next_token }

let parse xml =
  Some
    { resource_tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceTagList" xml)
             ResourceTagList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ResourceTagList.member", ResourceTagList.to_query v.resource_tag_list))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("ResourceTagList", ResourceTagList.to_json v.resource_tag_list)
       ])

let of_json j =
  { resource_tag_list =
      ResourceTagList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTagList"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
