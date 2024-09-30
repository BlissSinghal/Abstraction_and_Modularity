;; open Assert
;; stop_on_failure ()
(******************************************************************************)
(* PROBLEM 1: BINARY TREES                                                    *)
(******************************************************************************)

(* Below, we've defined a datatype that corresponds to a generic binary
   tree. These are like the binary trees you saw in lecture and in Homework 2,
   except that, instead of working only with helices and strings, these are
   generic, and so can contain any type. (Note that once you use a tree with
   data of a particular type, you can't add data of another type later on. So
   you can't put a string into an `int tree`, or vice versa.)

   In this file, we'll define some operations on binary trees and binary
   search trees. We have provided comprehensive testing for the more complex
   tree methods so that you only need to implement these functions. *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let leaf x = Node (Empty, x, Empty)

(* Here's an example binary tree.

       5
      / \
     7   9

   Its OCaml representation is below: *)

let example_tree: int tree =
  Node (
    Node (Empty, 7, Empty),
    5,
    Node (Empty, 9, Empty)
  )

(* We also provide the following function for you to visualize your trees!
   Feel free to use this while debugging your functions. For example, if 
   the tree looks like 
         7
        / \
       4   6
   Then, print_tree should return:
  Node ( 
    Node ( 
        Empty,
        4,
        Empty
    ),
    7,
    Node ( 
        Empty,
        6,
        Empty
    )
  )
    *)

let print_tree (t: int tree) : unit =
  let rec w d = if d = 0 then "" else " " ^ w (d - 1) in
  let rec aux t d =
    w (d * 4) ^
    match t with
    | Empty -> "Empty"
    | Node (lt, n, rt) ->
      "Node ( \n" ^
      aux lt (d + 1) ^ ",\n" ^
      w ((d + 1) * 4) ^ (string_of_int n) ^ ",\n" ^
      aux rt (d + 1) ^ "\n" ^
      w (d * 4) ^ ")"
  in print_string @@ "\n" ^ (aux t 0) ^ "\n"

(* Now it's your turn. Write the following tree:

         7
        / \
       4   6
      / \
     2   9
        / \
       5   8
 *)

let your_tree: int tree = 
Node (
  Node (
    Node (Empty, 2, Empty),
    4,
    Node (
      Node (Empty, 5, Empty),
      9,
      Node (Empty, 8, Empty)
    )
  ),
  7,
  Node (Empty, 6, Empty)
)


(* Let's define a "perfect" binary tree as one where:
   - every leaf is the same distance from the root
   - every node has either 0 or 2 children

   For example:

           19
          /  \
        52    16
       / \    / \
      5   2  4   1

   Write a function `is_perfect` that tests whether a tree is perfect.

   Hint: you'll probably want to write a helper function (e.g., one that
   calculates the height of a tree). *)

(*helper function to calc. the height of tree*)
let rec get_height (t: 'a tree) : int = 
  begin match t with
  |Empty -> 0
  |Node(rt, _, lt) -> 
    let left_height = 1 + get_height lt in
    let right_height = 1 + get_height rt in
    (*need to return the longer height*)
    if left_height > right_height then left_height
    else right_height
  end

let rec is_perfect (t: 'a tree) : bool =
  begin match t with 
  |Empty -> true
  |Node(Empty,_, Empty) -> true
  |Node(Empty, _, _) -> false
  |Node(_, _, Empty) -> false
  |Node(rt, x, lt) -> get_height lt = get_height rt 
    && is_perfect lt && is_perfect rt
  end

(* Here are some test cases. Make sure to write some of your own -- including
   some where the tree being tested is _not_ perfect! *)

let test () : bool =
  is_perfect Empty
;; run_test "is_perfect: empty tree returns true" test

let test () : bool =
  is_perfect (Node (Empty, 6, Empty))
;; run_test "is_perfect: leaf returns true" test

let test () : bool =
  let ex_tree = Node (Node (Node (Empty, 5, Empty), 52, Node (Empty, 2, Empty)),
    19, Node (Node (Empty, 4, Empty), 16, Node (Empty, 1, Empty))) in
  is_perfect ex_tree
;; run_test "is_perfect: perfect multi-level tree returns true" test


(* Next, write a function that finds the maximum element in a binary tree, as
   determined by the polymorphic function `max`, which can take in elements of
   various data types. (Note that your function should work for *all* binary
   trees, not just binary search trees.) Because an empty tree does not have a
   maximum element, you should call `failwith` if `tree_max` is passed an
   empty tree. *)

let rec tree_max (t: 'a tree) : 'a =
  begin match t with
  |Empty -> failwith "empty tree"
  | Node(Empty, x, Empty) -> x
  | Node(Empty, x, rt) -> 
    let right_max = tree_max rt in
    if right_max > x then right_max else x  
  |Node (lt, x, Empty) -> let left_max = tree_max lt in
    if left_max > x then left_max else x
  | Node (lt, x, rt) -> 
    let left_max = tree_max lt in
    let right_max = tree_max rt in
    if x >= left_max && x >= right_max then x
    else if left_max >= x && left_max >= right_max then left_max
    else right_max

  end

let test () : bool =
  tree_max Empty = 42
;; run_failing_test "tree_max: running on empty fails" test

let test () : bool =
  tree_max example_tree = 9
;; run_test "tree_max: depth-2 tree" test

(* Warning: These tests aren't exhaustive! Although your tests themselves will
   not be graded for this problem, it can be tricky to get right without
   thorough testing. *)


(******************************************************************************)
(* PROBLEM 2: BINARY SEARCH TREES                                             *)
(******************************************************************************)


(* Next, we will write some functions that operate on binary SEARCH
   trees. Recall that a binary search tree is a binary tree that
   follows some additional invariants:

   - `Empty` is a binary search tree, and
   - `Node (lt, v, rt)` is a binary search tree if both
     - `lt` is a binary search tree, and every value in `lt` is less than `v`
     - `rt` is a binary search tree, and every value in `rt` is greater than `v`

   Notice that this description is recursive, just like our datatype
   definition!

   You may assume that all of the trees that are provided to the functions in
   this problem satisfy this invariant. *)

(* NOTE: Again, many of the functions in the remainder of this file are
   available in the CIS 1200 lecture notes. Although it is okay to use the
   notes as a reference if you get stuck, you should ensure you _understand_
   them.  The best way to do this is to review a function in the notes (as
   needed), think about how it works, and then rewrite it from scratch without
   looking at the notes.

   We've provided several test cases for each of these functions, which
   exercise their functionality fairly thoroughly; however, you may still find
   it helpful to make some more tests of your own, to help you understand what
   the functions should be doing. *)

(* First, write a function called `lookup` that searches a generic binary
   search tree for a particular value. You should leverage the BST invariants
   here, so your implementation should NOT have to search every subtree. *)

let rec lookup (x: 'a) (t: 'a tree) : bool =
  begin match t with 
  |Empty -> false
  |Node (lt, value, rt) -> value = x ||
    if x > value then lookup x rt
    else lookup x lt
  end

let test () : bool =
  not (lookup 7 example_tree)
;; run_test "lookup: assumes BST invariants" test

let test () : bool =
  let t = Node (Node (Empty, 1, Empty), 2,  Node (Empty, 3, Empty)) in
  not (lookup 5 t)
;; run_test "lookup: not in tree" test

let test () : bool =
  let t = Node (Node (Empty, 2, Empty), 5,  Node (Empty, 7, Empty)) in
  lookup 2 t
;; run_test "lookup: element in tree" test

(* The next function should return all of the elements of a binary search
   tree, sorted in ascending order. This is called the "in-order traversal" of
   a BST. *)

let rec inorder (t: 'a tree) : 'a list =
  (*need to go through the left subtree first*)
  begin match t with 
  |Empty -> []
  |Node (lt, x, rt) -> (inorder lt) @ (x::inorder rt)
  end
let test () : bool =
  inorder Empty = []
;; run_test "inorder: empty" test

let test () : bool =
  inorder (Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty))) = [1; 2; 3]
;; run_test "inorder: regular BST" test

(* Write the function that inserts an element into a binary search tree. *)

let rec insert (x: 'a) (t: 'a tree) : 'a tree =
  begin match t with 
  |Empty -> Node(Empty, x, Empty)
  |Node(lt, num, rt) -> 
    (*if x = num then just returing the same tree*)
    if x = num then t
    (*if x is less than num, then looking thru left tree*)
    else if x < num then Node(insert x lt, num, rt)
    else Node(lt, num, insert x rt)
  end

let test () : bool =
  insert 2 Empty = Node (Empty, 2, Empty)
;; run_test "insert: empty" test

let test () : bool =
  insert 2 (Node (Empty, 2, Empty)) = Node (Empty, 2, Empty)
;; run_test "insert: already in tree" test

let test () : bool =
  let t = (Node (Node (Empty, 1, Empty), 2, Node (Empty, 4, Empty))) in
  let t' =
    Node (Node (Empty, 1, Empty), 2, Node (Node (Empty, 3, Empty), 4, Empty))
  in
  insert 3 t = t'
;; run_test "insert: not in tree" test


(* Now, use `insert` to write the function `tree_of_list`. *)

let rec tree_of_list (l: 'a list) : 'a tree =
  begin match l with
  (*if empty list, creating empty node in tree*) 
  |[] -> Empty
  |h::t -> insert h (tree_of_list t)
  end

let test () : bool =
  tree_of_list [2; 4] = Node (Node (Empty, 2, Empty), 4, Empty)
;; run_test "tree_of_list: depth-2 tree" test

let test () : bool =
  tree_of_list [5; 2; 4] =
    Node (Node (Empty, 2, Empty), 4, Node (Empty, 5, Empty))
;; run_test "tree_of_list: multiple leaves" test


(* The `delete` function returns a tree that is like `t` except with the
   element `x` removed.  If `x` is not present, the resulting tree has the
   same shape as `t`. *)


let rec delete (x: 'a) (t: 'a tree) : 'a tree =
  begin match t with 
  |Empty -> t
  |Node(lt, num, rt) ->
    if x < num then Node(delete x lt, num, rt)
    else if x > num then Node(lt, num, delete x rt)
    else 
      begin match Node(lt, num, rt) with
      |Node(Empty, _, Empty) -> Empty
      |Node(Empty, _, rt) -> rt
      |Node(lt, _, Empty) -> lt
      |Node(lt,_, rt) ->
        (*find the max of the left subtree and make it the root node*)
        let lt_max = tree_max lt in
        Node(delete lt_max lt, lt_max, rt)
      end
  end

let test () : bool =
  delete 3 Empty = Empty
;; run_test "delete: empty" test

let test () : bool =
  delete 2 (Node (Node (Empty, 2, Empty), 5, Node (Empty, 7, Empty))) =
    Node (Empty, 5, Node (Empty, 7, Empty))
;; run_test "delete: element in tree" test

let test () : bool =
  let t = (Node (Node (Empty, 2, Empty), 5, Node (Empty, 7, Empty))) in
  delete 1 t = t
;; run_test "delete: element not in tree" test
