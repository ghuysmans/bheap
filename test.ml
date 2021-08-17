module Counter(X : Binary_heap.Ordered) = struct
  type t = X.t
  let value = ref 0
  let compare x y = incr value; X.compare x y
end

(* quick test of Binary_heap *)

module E = Counter(struct
  type t = int
  let compare = Stdlib.compare
end)

let measure f =
  E.value := 0;
  let x = f () in
  !E.value, x

module H = Binary_heap.Make(E)

let dummy = 1729
let h = H.create ~dummy 0
let () = assert (H.is_empty h)
let () = assert (H.length h = 0)
let () = H.add h 42
let () = assert (not (H.is_empty h))
let () = assert (H.length h = 1)
let () = assert (H.minimum h = 42)
let x = H.pop_minimum h
let () = assert (x = 42)
let () = assert (H.is_empty h)

let () = for i = 200 downto -200 do H.add h i done
let () = assert (H.minimum h = -200)
let () = assert (H.length h = 401)
let () = for i = -200 to 200 do
           assert (H.minimum h = i);
           let x = H.pop_minimum h in
           assert (x = i);
           assert (H.length h = 200 - i)
         done

let () =
  let h = H.create ~dummy 10 in
  for x = 200 downto 1 do H.add h x done;
  assert (H.length h = 200);
  let v = 201 in
  for x = 1 to 200 do
    assert (H.minimum h = x);
    H.remove_and_add h v; assert (H.length h = 200);
  done;
  assert (H.fold (+) h 0 = 200 * v);
  for _ = 1 to 200 do assert (H.pop_minimum h = v) done

let () =
  let h = H.create ~dummy 42 in
  for _ = 1 to 1000 do
    if Random.bool () then H.add h (Random.int 1000);
    if not (H.is_empty h) && Random.int 3 = 0 then H.remove h
  done;
  for _ = 1 to 1000 do
    if Random.bool () then H.add h (Random.int 1000);
    if not (H.is_empty h) && Random.int 3 < 2 then H.remove h
  done;
  Format.eprintf "%d@." (H.length h)

let rev arr =
  let len = Array.length arr in
  for i = 0 to len / 2 - 1 do
    let tmp = arr.(i) in
    arr.(i) <- arr.(len - i - 1);
    arr.(len - i - 1) <- tmp
  done

let benchmark () =
  let size = ref 1 in
  Format.printf "n,add,heapify,revsort,native,stable,exp,ratio@.";
  while !size <= 1000000 do
    let arr = Array.init !size (fun _ -> Random.int 1000) in
    let add, () = measure (fun () ->
      let h = H.create ~dummy 10 in
      for i = 0 to !size - 1 do
        H.add h arr.(i)
      done
    ) in
    let heapify, _ = measure (fun () -> H.heapify (Array.copy arr)) in
    let native, e = measure Array.(fun () -> let a = copy arr in sort E.compare a; a) in
    let stable, _ = measure (fun () -> Array.stable_sort E.compare (Array.copy arr)) in
    let revsort, _ = measure (fun () -> H.rev_sort ~dummy:0 arr) in
    rev e; assert (arr = e);
    let log2 x = log x /. log 2. in
    let expected = float !size *. log2 (float !size) in
    Format.printf "%d,%d,%d,%d,%d,%d,%.0f,%.0f@." !size add heapify revsort
      native stable expected (float revsort /. expected);
    size := !size * 10
  done

let () =
  let l =
    let gen _ =
      let a = Array.init 100 (fun _ -> Random.int 10000) in
      Array.sort compare a;
      Array.to_list a
    in
    Array.(init 10 gen |> to_list)
  in
  let r = ref [] in
  let prepend x = r := x :: !r in
  let f l = Binary_heap.Stream.(of_list l |> cache) in
  Binary_heap.merge (module E) (List.map f l) prepend;
  assert List.(sort compare (flatten l) = rev !r)
  (* TODO benchmark *)
