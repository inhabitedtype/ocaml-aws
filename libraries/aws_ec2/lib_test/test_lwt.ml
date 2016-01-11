open OUnit
open Aws_ec2

module T = Test.TestSuite(struct
    let access_key = Unix.getenv "AWS_ACCESS_KEY"
    let secret_key = Unix.getenv "AWS_SECRET_KEY"

    type 'a m = 'a Lwt.t
    let run_request = Aws_lwt.Runtime.run_request ~access_key ~secret_key
    let un_m = Lwt_main.run
  end)
