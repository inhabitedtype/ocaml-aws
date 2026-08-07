open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; fpga_image_id : String.t
  }

let make ?dry_run ~fpga_image_id () = { dry_run; fpga_image_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; fpga_image_id =
        Aws.Xml.required
          "FpgaImageId"
          (Aws.Util.option_bind (Aws.Xml.member "FpgaImageId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("FpgaImageId", String.to_query v.fpga_image_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("FpgaImageId", String.to_json v.fpga_image_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; fpga_image_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FpgaImageId"))
  }
