open Aws.BaseTypes

type t =
  { cache_subnet_group_name : String.t
  ; cache_subnet_group_description : String.t option
  ; subnet_ids : SubnetIdentifierList.t
  }

let make ~cache_subnet_group_name ?cache_subnet_group_description ?(subnet_ids = []) () =
  { cache_subnet_group_name; cache_subnet_group_description; subnet_ids }

let parse xml =
  Some
    { cache_subnet_group_name =
        Aws.Xml.required
          "CacheSubnetGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse)
    ; cache_subnet_group_description =
        Aws.Util.option_bind
          (Aws.Xml.member "CacheSubnetGroupDescription" xml)
          String.parse
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetIds" xml)
             SubnetIdentifierList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("SubnetIds.member", SubnetIdentifierList.to_query v.subnet_ids))
       ; Aws.Util.option_map v.cache_subnet_group_description (fun f ->
             Aws.Query.Pair ("CacheSubnetGroupDescription", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CacheSubnetGroupName", String.to_query v.cache_subnet_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SubnetIds", SubnetIdentifierList.to_json v.subnet_ids)
       ; Aws.Util.option_map v.cache_subnet_group_description (fun f ->
             "CacheSubnetGroupDescription", String.to_json f)
       ; Some ("CacheSubnetGroupName", String.to_json v.cache_subnet_group_name)
       ])

let of_json j =
  { cache_subnet_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSubnetGroupName"))
  ; cache_subnet_group_description =
      Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupDescription") String.of_json
  ; subnet_ids =
      SubnetIdentifierList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetIds"))
  }
