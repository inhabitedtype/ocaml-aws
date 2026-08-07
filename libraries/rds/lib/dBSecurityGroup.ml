open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; d_b_security_group_name : String.t option
  ; d_b_security_group_description : String.t option
  ; vpc_id : String.t option
  ; e_c2_security_groups : EC2SecurityGroupList.t
  ; i_p_ranges : IPRangeList.t
  ; d_b_security_group_arn : String.t option
  }

let make
    ?owner_id
    ?d_b_security_group_name
    ?d_b_security_group_description
    ?vpc_id
    ?(e_c2_security_groups = [])
    ?(i_p_ranges = [])
    ?d_b_security_group_arn
    () =
  { owner_id
  ; d_b_security_group_name
  ; d_b_security_group_description
  ; vpc_id
  ; e_c2_security_groups
  ; i_p_ranges
  ; d_b_security_group_arn
  }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "OwnerId" xml) String.parse
    ; d_b_security_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBSecurityGroupName" xml) String.parse
    ; d_b_security_group_description =
        Aws.Util.option_bind
          (Aws.Xml.member "DBSecurityGroupDescription" xml)
          String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; e_c2_security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EC2SecurityGroups" xml)
             EC2SecurityGroupList.parse)
    ; i_p_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "IPRanges" xml) IPRangeList.parse)
    ; d_b_security_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBSecurityGroupArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_security_group_arn (fun f ->
             Aws.Query.Pair ("DBSecurityGroupArn", String.to_query f))
       ; Some (Aws.Query.Pair ("IPRanges.member", IPRangeList.to_query v.i_p_ranges))
       ; Some
           (Aws.Query.Pair
              ( "EC2SecurityGroups.member"
              , EC2SecurityGroupList.to_query v.e_c2_security_groups ))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.d_b_security_group_description (fun f ->
             Aws.Query.Pair ("DBSecurityGroupDescription", String.to_query f))
       ; Aws.Util.option_map v.d_b_security_group_name (fun f ->
             Aws.Query.Pair ("DBSecurityGroupName", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_security_group_arn (fun f ->
             "DBSecurityGroupArn", String.to_json f)
       ; Some ("IPRanges", IPRangeList.to_json v.i_p_ranges)
       ; Some ("EC2SecurityGroups", EC2SecurityGroupList.to_json v.e_c2_security_groups)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.d_b_security_group_description (fun f ->
             "DBSecurityGroupDescription", String.to_json f)
       ; Aws.Util.option_map v.d_b_security_group_name (fun f ->
             "DBSecurityGroupName", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "OwnerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "OwnerId") String.of_json
  ; d_b_security_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBSecurityGroupName") String.of_json
  ; d_b_security_group_description =
      Aws.Util.option_map (Aws.Json.lookup j "DBSecurityGroupDescription") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; e_c2_security_groups =
      EC2SecurityGroupList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EC2SecurityGroups"))
  ; i_p_ranges =
      IPRangeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IPRanges"))
  ; d_b_security_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBSecurityGroupArn") String.of_json
  }
