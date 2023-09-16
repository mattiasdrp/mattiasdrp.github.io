module SMap = Map.Make (String)

module Joueuse = struct
  type t = { numero : int; position : string; groupe : string; niveau : string }
end

let parse ci =
  let open Joueuse in
  let rec aux niveau groupe position joueuses =
    match String.split_on_char ';' (input_line ci) with
    | info :: numero :: nom :: _ -> (
        match (niveau, groupe, position, info, numero, nom) with
        | None, _, _, _, _, _ -> aux (Some info) groupe position joueuses
        | _, None, _, _, _, _ -> aux niveau (Some info) position joueuses
        | _, _, _, "", "", "" -> aux None None None joueuses
        | _, _, _, groupe, "", "" -> aux niveau (Some groupe) None joueuses
        | _, _, _, "", numero, nom ->
            let joueuses =
              SMap.add nom
                {
                  numero = int_of_string numero;
                  position = Option.get position;
                  groupe = Option.get groupe;
                  niveau = Option.get niveau;
                }
                joueuses
            in
            aux niveau groupe position joueuses
        | _, _, _, position, numero, nom ->
            let joueuses =
              SMap.add nom
                {
                  numero = int_of_string numero;
                  position;
                  groupe = Option.get groupe;
                  niveau = Option.get niveau;
                }
                joueuses
            in
            aux niveau groupe (Some position) joueuses)
    | _ -> failwith "Your data doesn't have the proper format."
    | exception End_of_file ->
        close_in ci;
        joueuses
  in
  aux None None None SMap.empty

let fill_buckets n groupes =
  let buckets = Array.init n (fun _ -> []) in
  let rec aux curr_bucket joueuses j_size groupe id_groupes position
      id_positions =
    (* No more players in this position  *)
    if j_size = 0 then
      match
        let _groupe, positions = groupes.(id_groupes) in
        positions.(id_positions + 1)
      with
      | position, joueuses ->
          aux curr_bucket joueuses (Array.length joueuses) groupe id_groupes
            position (id_positions + 1)
      | exception Invalid_argument _ -> (
          match groupes.(id_groupes + 1) with
          | groupe, positions ->
              let position, joueuses = positions.(0) in
              aux curr_bucket joueuses (Array.length joueuses) groupe
                (id_groupes + 1) position 0
          | exception Invalid_argument _ -> buckets)
    else
      let index_j = Random.int j_size in
      let joueuse = joueuses.(index_j) in
      joueuses.(index_j) <- joueuses.(j_size - 1);
      buckets.(curr_bucket) <-
        (joueuse, groupe, position) :: buckets.(curr_bucket);
      aux
        ((curr_bucket + 1) mod n)
        joueuses (j_size - 1) groupe id_groupes position id_positions
  in
  let groupe, positions = groupes.(0) in
  let position, joueuses = positions.(0) in
  aux 0 joueuses (Array.length joueuses) groupe 0 position 0

let generate_buckets file =
  let ci = open_in file in
  let joueuses = parse ci in
  let groupes =
    SMap.fold
      (fun joueuse Joueuse.{ numero; position; groupe; niveau } groupes ->
        SMap.update groupe
          (function
            | Some g ->
                Some
                  (SMap.update position
                     (function
                       | Some p -> Some ((joueuse, numero, niveau) :: p)
                       | None -> Some [ (joueuse, numero, niveau) ])
                     g)
            | None ->
                Some (SMap.singleton position [ (joueuse, numero, niveau) ]))
          groupes)
      joueuses SMap.empty
  in
  let groupes =
    SMap.fold
      (fun groupe positions acc ->
        let positions =
          SMap.fold
            (fun position joueuses acc ->
              (position, Array.of_list joueuses) :: acc)
            positions []
          |> Array.of_list
        in
        (groupe, positions) :: acc)
      groupes []
    |> Array.of_list
  in
  Array.iter
    (fun (groupe, positions) ->
      Format.printf "@[<v 2>%s:@," groupe;
      Array.iter
        (fun (position, joueuses) ->
          Format.printf "@[<v 2>%s:" position;
          Array.iter
            (fun (joueuse, numero, niveau) ->
              Format.printf "@,(%s, %d, %s)" joueuse numero niveau)
            joueuses;
          Format.printf "@]@,")
        positions;
      Format.printf "@]@,")
    groupes;
  let buckets = fill_buckets 3 groupes in
  Array.iteri
    (fun i joueuses ->
      Format.printf "%d: @[<v 0>" (i + 1);
      List.iter
        (fun ((joueuse, _, niveau), groupe, position) ->
          Format.printf "@,%s: %s, %s, %s)" joueuse groupe position niveau)
        joueuses;
      Format.printf "@]@,@,")

    buckets

let () =
  Random.self_init ();
  generate_buckets "test-equipe-aleatoire.csv"


let root_component = Bonsai.const (Bonsai_web.Vdom.Node.text "hello world")
open Bonsai_web

let () =
  Start.start  (* ~bind_to_element_with_id:"app" *) root_component