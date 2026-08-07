open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; cache_security_group_name : String.t option
  ; description : String.t option
  ; e_c2_security_groups : EC2SecurityGroupList.t
  ; a_r_n : String.t option
  }

let make
    ?owner_id
    ?cache_security_group_name
    ?description
    ?(e_c2_security_groups = [])
    ?a_r_n
    () =
  { owner_id; cache_security_group_name; description; e_c2_security_groups; a_r_n }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "OwnerId" xml) String.parse
    ; cache_security_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheSecurityGroupName" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; e_c2_security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EC2SecurityGroups" xml)
             EC2SecurityGroupList.parse)
    ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.a_r_n (fun f -> Aws.Query.Pair ("ARN", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "EC2SecurityGroups.member"
              , EC2SecurityGroupList.to_query v.e_c2_security_groups ))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.cache_security_group_name (fun f ->
             Aws.Query.Pair ("CacheSecurityGroupName", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
       ; Some ("EC2SecurityGroups", EC2SecurityGroupList.to_json v.e_c2_security_groups)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.cache_security_group_name (fun f ->
             "CacheSecurityGroupName", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "OwnerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "OwnerId") String.of_json
  ; cache_security_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheSecurityGroupName") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; e_c2_security_groups =
      EC2SecurityGroupList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EC2SecurityGroups"))
  ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
  }
