open Aws.BaseTypes

type t =
  { cache_security_group_name : String.t
  ; description : String.t
  ; tags : TagList.t
  }

let make ~cache_security_group_name ~description ?(tags = []) () =
  { cache_security_group_name; description; tags }

let parse xml =
  Some
    { cache_security_group_name =
        Aws.Xml.required
          "CacheSecurityGroupName"
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheSecurityGroupName" xml)
             String.parse)
    ; description =
        Aws.Xml.required
          "Description"
          (Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("Description", String.to_query v.description))
       ; Some
           (Aws.Query.Pair
              ("CacheSecurityGroupName", String.to_query v.cache_security_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("Description", String.to_json v.description)
       ; Some ("CacheSecurityGroupName", String.to_json v.cache_security_group_name)
       ])

let of_json j =
  { cache_security_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupName"))
  ; description =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Description"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
