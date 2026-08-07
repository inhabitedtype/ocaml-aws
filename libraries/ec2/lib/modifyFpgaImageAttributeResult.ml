type t = { fpga_image_attribute : FpgaImageAttribute.t option }

let make ?fpga_image_attribute () = { fpga_image_attribute }

let parse xml =
  Some
    { fpga_image_attribute =
        Aws.Util.option_bind
          (Aws.Xml.member "fpgaImageAttribute" xml)
          FpgaImageAttribute.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fpga_image_attribute (fun f ->
             Aws.Query.Pair ("FpgaImageAttribute", FpgaImageAttribute.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fpga_image_attribute (fun f ->
             "fpgaImageAttribute", FpgaImageAttribute.to_json f)
       ])

let of_json j =
  { fpga_image_attribute =
      Aws.Util.option_map
        (Aws.Json.lookup j "fpgaImageAttribute")
        FpgaImageAttribute.of_json
  }
