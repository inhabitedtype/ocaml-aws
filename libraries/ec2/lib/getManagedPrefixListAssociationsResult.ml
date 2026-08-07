open Aws.BaseTypes

type t =
  { prefix_list_associations : PrefixListAssociationSet.t
  ; next_token : String.t option
  }

let make ?(prefix_list_associations = []) ?next_token () =
  { prefix_list_associations; next_token }

let parse xml =
  Some
    { prefix_list_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "prefixListAssociationSet" xml)
             PrefixListAssociationSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PrefixListAssociationSet"
              , PrefixListAssociationSet.to_query v.prefix_list_associations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "prefixListAssociationSet"
           , PrefixListAssociationSet.to_json v.prefix_list_associations )
       ])

let of_json j =
  { prefix_list_associations =
      PrefixListAssociationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "prefixListAssociationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
