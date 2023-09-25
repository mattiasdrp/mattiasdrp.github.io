open Lib

let root_component = Bonsai.const (Bonsai_web.Vdom.Node.text "hello world")

open Bonsai_web

let _f file =
  let groupes = read file in
  let buckets = generate_buckets 3 groupes in
  Format.eprintf "%a@." pp_buckets buckets

let () =
  Random.self_init ();
  Start.start ~bind_to_element_with_id:"app" root_component
