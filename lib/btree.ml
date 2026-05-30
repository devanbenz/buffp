type ('a, 'b) btree = {
  root : bool;
  children : ('a, 'b) btree option array;
  key : 'a;
  value : 'b;
}

let create ~page_size ~root key value =
  { root; children = Array.make page_size None; key; value }

let add_child node i child =
  match node.children.(i) with
  | None ->
      node.children.(i) <- Some child;
      Some child
  | Some child -> None

let rec insert ~root ~node =
  assert (Array.length root.children > 0);
  if root.children.(0) = None then root.children.(0) <- Some node
