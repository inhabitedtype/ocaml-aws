open OUnit
open Aws_ec2

module T = Test.TestSuite(struct
    let access_key = Unix.getenv "AWS_ACCESS_KEY"
    let secret_key = Unix.getenv "AWS_SECRET_KEY"

    type 'a m = 'a Async.Std.Deferred.t
    let run_request = Aws_async.Runtime.run_request ~access_key ~secret_key
    let un_m v = Async.Std.Thread_safe.block_on_async_exn (fun () -> v)
  end)
