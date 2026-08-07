open Aws.BaseTypes

type t =
  { d_b_subnet_group_name : String.t
  ; d_b_subnet_group_description : String.t
  ; subnet_ids : SubnetIdentifierList.t
  ; tags : TagList.t
  }

let make ~d_b_subnet_group_name ~d_b_subnet_group_description ~subnet_ids ?(tags = []) ()
    =
  { d_b_subnet_group_name; d_b_subnet_group_description; subnet_ids; tags }

let parse xml =
  Some
    { d_b_subnet_group_name =
        Aws.Xml.required
          "DBSubnetGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupName" xml) String.parse)
    ; d_b_subnet_group_description =
        Aws.Xml.required
          "DBSubnetGroupDescription"
          (Aws.Util.option_bind
             (Aws.Xml.member "DBSubnetGroupDescription" xml)
             String.parse)
    ; subnet_ids =
        Aws.Xml.required
          "SubnetIds"
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetIds" xml)
             SubnetIdentifierList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair ("SubnetIds.member", SubnetIdentifierList.to_query v.subnet_ids))
       ; Some
           (Aws.Query.Pair
              ("DBSubnetGroupDescription", String.to_query v.d_b_subnet_group_description))
       ; Some
           (Aws.Query.Pair ("DBSubnetGroupName", String.to_query v.d_b_subnet_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("SubnetIds", SubnetIdentifierList.to_json v.subnet_ids)
       ; Some ("DBSubnetGroupDescription", String.to_json v.d_b_subnet_group_description)
       ; Some ("DBSubnetGroupName", String.to_json v.d_b_subnet_group_name)
       ])

let of_json j =
  { d_b_subnet_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSubnetGroupName"))
  ; d_b_subnet_group_description =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSubnetGroupDescription"))
  ; subnet_ids =
      SubnetIdentifierList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetIds"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
