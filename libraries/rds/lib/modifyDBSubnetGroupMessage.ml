open Aws.BaseTypes

type t =
  { d_b_subnet_group_name : String.t
  ; d_b_subnet_group_description : String.t option
  ; subnet_ids : SubnetIdentifierList.t
  }

let make ~d_b_subnet_group_name ?d_b_subnet_group_description ~subnet_ids () =
  { d_b_subnet_group_name; d_b_subnet_group_description; subnet_ids }

let parse xml =
  Some
    { d_b_subnet_group_name =
        Aws.Xml.required
          "DBSubnetGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupName" xml) String.parse)
    ; d_b_subnet_group_description =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupDescription" xml) String.parse
    ; subnet_ids =
        Aws.Xml.required
          "SubnetIds"
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetIds" xml)
             SubnetIdentifierList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("SubnetIds.member", SubnetIdentifierList.to_query v.subnet_ids))
       ; Aws.Util.option_map v.d_b_subnet_group_description (fun f ->
             Aws.Query.Pair ("DBSubnetGroupDescription", String.to_query f))
       ; Some
           (Aws.Query.Pair ("DBSubnetGroupName", String.to_query v.d_b_subnet_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SubnetIds", SubnetIdentifierList.to_json v.subnet_ids)
       ; Aws.Util.option_map v.d_b_subnet_group_description (fun f ->
             "DBSubnetGroupDescription", String.to_json f)
       ; Some ("DBSubnetGroupName", String.to_json v.d_b_subnet_group_name)
       ])

let of_json j =
  { d_b_subnet_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSubnetGroupName"))
  ; d_b_subnet_group_description =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroupDescription") String.of_json
  ; subnet_ids =
      SubnetIdentifierList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetIds"))
  }
