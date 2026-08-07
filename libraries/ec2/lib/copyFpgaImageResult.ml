open Aws.BaseTypes

type t = { fpga_image_id : String.t option }

let make ?fpga_image_id () = { fpga_image_id }

let parse xml =
  Some
    { fpga_image_id = Aws.Util.option_bind (Aws.Xml.member "fpgaImageId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fpga_image_id (fun f ->
             Aws.Query.Pair ("FpgaImageId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fpga_image_id (fun f -> "fpgaImageId", String.to_json f) ])

let of_json j =
  { fpga_image_id = Aws.Util.option_map (Aws.Json.lookup j "fpgaImageId") String.of_json }
