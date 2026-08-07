open Aws.BaseTypes

type t =
  { association_id : String.t option
  ; cidr_block : String.t option
  ; cidr_block_state : VpcCidrBlockState.t option
  }

let make ?association_id ?cidr_block ?cidr_block_state () =
  { association_id; cidr_block; cidr_block_state }

let parse xml =
  Some
    { association_id =
        Aws.Util.option_bind (Aws.Xml.member "associationId" xml) String.parse
    ; cidr_block = Aws.Util.option_bind (Aws.Xml.member "cidrBlock" xml) String.parse
    ; cidr_block_state =
        Aws.Util.option_bind (Aws.Xml.member "cidrBlockState" xml) VpcCidrBlockState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_block_state (fun f ->
             Aws.Query.Pair ("CidrBlockState", VpcCidrBlockState.to_query f))
       ; Aws.Util.option_map v.cidr_block (fun f ->
             Aws.Query.Pair ("CidrBlock", String.to_query f))
       ; Aws.Util.option_map v.association_id (fun f ->
             Aws.Query.Pair ("AssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_block_state (fun f ->
             "cidrBlockState", VpcCidrBlockState.to_json f)
       ; Aws.Util.option_map v.cidr_block (fun f -> "cidrBlock", String.to_json f)
       ; Aws.Util.option_map v.association_id (fun f -> "associationId", String.to_json f)
       ])

let of_json j =
  { association_id =
      Aws.Util.option_map (Aws.Json.lookup j "associationId") String.of_json
  ; cidr_block = Aws.Util.option_map (Aws.Json.lookup j "cidrBlock") String.of_json
  ; cidr_block_state =
      Aws.Util.option_map (Aws.Json.lookup j "cidrBlockState") VpcCidrBlockState.of_json
  }
