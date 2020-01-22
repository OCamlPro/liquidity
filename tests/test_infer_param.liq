type storage = tez * string
let%entry default () s  = [], s

(* type storage = int
 *
 * let%entry e1
 *       parameter
 *       _storage =
 *   let f (x : int) = x in
 *   ( [], 1 + f parameter )
 *
 * let%entry e2
 *       parameter
 *       storage =
 *   ( [], if parameter then storage else 0 ) *)
