open Aws.BaseTypes

type t =
  { cache_security_group_name : String.t
  ; e_c2_security_group_name : String.t
  ; e_c2_security_group_owner_id : String.t
  }

let make
    ~cache_security_group_name
    ~e_c2_security_group_name
    ~e_c2_security_group_owner_id
    () =
  { cache_security_group_name; e_c2_security_group_name; e_c2_security_group_owner_id }

let parse xml =
  Some
    { cache_security_group_name =
        Aws.Xml.required
          "CacheSecurityGroupName"
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheSecurityGroupName" xml)
             String.parse)
    ; e_c2_security_group_name =
        Aws.Xml.required
          "EC2SecurityGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "EC2SecurityGroupName" xml) String.parse)
    ; e_c2_security_group_owner_id =
        Aws.Xml.required
          "EC2SecurityGroupOwnerId"
          (Aws.Util.option_bind
             (Aws.Xml.member "EC2SecurityGroupOwnerId" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("EC2SecurityGroupOwnerId", String.to_query v.e_c2_security_group_owner_id))
       ; Some
           (Aws.Query.Pair
              ("EC2SecurityGroupName", String.to_query v.e_c2_security_group_name))
       ; Some
           (Aws.Query.Pair
              ("CacheSecurityGroupName", String.to_query v.cache_security_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("EC2SecurityGroupOwnerId", String.to_json v.e_c2_security_group_owner_id)
       ; Some ("EC2SecurityGroupName", String.to_json v.e_c2_security_group_name)
       ; Some ("CacheSecurityGroupName", String.to_json v.cache_security_group_name)
       ])

let of_json j =
  { cache_security_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupName"))
  ; e_c2_security_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EC2SecurityGroupName"))
  ; e_c2_security_group_owner_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EC2SecurityGroupOwnerId"))
  }
