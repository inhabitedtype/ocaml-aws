open Aws.BaseTypes

type t =
  { cache_subnet_group_name : String.t
  ; cache_subnet_group_description : String.t
  ; subnet_ids : SubnetIdentifierList.t
  ; tags : TagList.t
  }

let make
    ~cache_subnet_group_name
    ~cache_subnet_group_description
    ~subnet_ids
    ?(tags = [])
    () =
  { cache_subnet_group_name; cache_subnet_group_description; subnet_ids; tags }

let parse xml =
  Some
    { cache_subnet_group_name =
        Aws.Xml.required
          "CacheSubnetGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse)
    ; cache_subnet_group_description =
        Aws.Xml.required
          "CacheSubnetGroupDescription"
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheSubnetGroupDescription" xml)
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
              ( "CacheSubnetGroupDescription"
              , String.to_query v.cache_subnet_group_description ))
       ; Some
           (Aws.Query.Pair
              ("CacheSubnetGroupName", String.to_query v.cache_subnet_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("SubnetIds", SubnetIdentifierList.to_json v.subnet_ids)
       ; Some
           ("CacheSubnetGroupDescription", String.to_json v.cache_subnet_group_description)
       ; Some ("CacheSubnetGroupName", String.to_json v.cache_subnet_group_name)
       ])

let of_json j =
  { cache_subnet_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSubnetGroupName"))
  ; cache_subnet_group_description =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSubnetGroupDescription"))
  ; subnet_ids =
      SubnetIdentifierList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetIds"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
