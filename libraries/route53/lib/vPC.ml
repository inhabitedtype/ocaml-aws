open Aws.BaseTypes

type t =
  { v_p_c_region : VPCRegion.t option
  ; v_p_c_id : String.t option
  }

let make ?v_p_c_region ?v_p_c_id () = { v_p_c_region; v_p_c_id }

let parse xml =
  Some
    { v_p_c_region = Aws.Util.option_bind (Aws.Xml.member "VPCRegion" xml) VPCRegion.parse
    ; v_p_c_id = Aws.Util.option_bind (Aws.Xml.member "VPCId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.v_p_c_id (fun f ->
             Aws.Query.Pair ("VPCId", String.to_query f))
       ; Aws.Util.option_map v.v_p_c_region (fun f ->
             Aws.Query.Pair ("VPCRegion", VPCRegion.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.v_p_c_id (fun f -> "VPCId", String.to_json f)
       ; Aws.Util.option_map v.v_p_c_region (fun f -> "VPCRegion", VPCRegion.to_json f)
       ])

let of_json j =
  { v_p_c_region = Aws.Util.option_map (Aws.Json.lookup j "VPCRegion") VPCRegion.of_json
  ; v_p_c_id = Aws.Util.option_map (Aws.Json.lookup j "VPCId") String.of_json
  }
