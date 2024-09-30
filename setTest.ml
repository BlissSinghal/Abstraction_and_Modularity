(******************************************************************************)
(* PROBLEM 5: WRITING TEST CASES                                              *)
(******************************************************************************)

(* The module `SetTest` defined below is a reuseable component that we'll use
   to test other modules that conform to the `SET` interface. When `SetTest`
   is instantiated with a particular set implementation, it will run all of
   its test cases against the set type defined in that implementation.  This
   means that the _same_ tests can be used for both the OrderedListSet and
   BSTSet implementations -- this makes sense because all implementations of
   `SET` should behave the same!

   Read through the module, then write your test cases in the space provided
   below. Make sure NOT to test for structural equality with sets.  Instead,
   use the equals function specified in the interface.  Your TAs will be
   grading the completeness of your tests. *)

module SetTest (SetImpl: SetInterface.SET) = struct
  ;; open SetImpl

  (* We first redefine the `run_test` and `run_failing_test` functions so that
     they prepend the name of the set we're testing to the test description. *)

  let run_test desc = Assert.run_test (debug_name ^ ": " ^ desc)
  let run_failing_test desc = Assert.run_failing_test (debug_name ^ ": " ^ desc)

  ;; print_endline ("\n--- Running tests for " ^ debug_name ^ " ---")

  (* Here are a couple of test cases to help get you started... *)
  (*testing the is_empty func*)
  let test () : bool =
    is_empty empty
  ;; run_test "is_empty: call on empty returns true" test

  let test() : bool = 
    let s = set_of_list [1] in
    not (is_empty s)
  ;; run_test "is_empty singleton" test 
  

  (* Note that some tests in this test module (such as the one below) may not
     pass until all the functions they depend on are implemented. For
     instance, the test below will fail for sets whose `set_of_list` function
     is not yet implemented (even if `is_empty` is correct).  This is fine:
     the goal here is just to record all the tests that we expect will pass
     when we get around to implementing everything later. *)

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    not (is_empty s)
  ;; run_test "is_empty: non-empty set returns false" test

  (* This is another case where the test won't pass since the "equals"
     function hasn't been implemented yet. We would like you to complete
     this test to confirm your understanding on "=" vs "equals". What should
     you use for this test to pass? *)



(*since these are sets, no longer lists, we have to use the equals comparison*)
   let test() : bool = 
      (* Uncomment the two lines below *)
      let s1 = add 1 (add 2 empty) in 
      let s2 = set_of_list [2; 1] in 
      equals s1 s2
      (* substitute the above line with the appropiate comparison 
         between s1 and s2 *)
   ;; run_test "make two sets equal" test

  (*let test() : bool = 
    let s1 = set_of_list [] in 
    let s2 = empty in 
    equals s1 s2
  ;; run_test "equals empty sets" test *)

(* Now, it's your turn! Make sure to comprehensively test all the other
   functions defined in the `SET` interface. It will probably be helpful to
   have the file `setInterface.ml` open as you work.  Your tests should stress
   the abstract properties of what it means to be a set, as well as the
   relationships among the operations provided by the SET interface.

   One thing to be careful of: your tests should not use `=` to compare sets:
   use the `equals` function instead.

   We strongly advise you to write tests for the functions in the order they
   appear in the interface. Write tests for all of the functions here before
   you start implementing. After the tests are written, you should be able to
   implement the functions one at a time in the same order and see your tests
   incrementally pass.

   Your TAs will be manually grading the completeness of your test cases. *)

  (* ---------- Write your own test cases below. ---------- *)
  (*testing the is_empty func for singletons bc already tested for empty and 
  multiple elements*)
    let test() : bool = 
    let s = set_of_list [1] in
    not (is_empty s)
  ;; run_test "is_empty singleton" test 

  (*testing the list of set func*)
  let test(): bool = 
    list_of_set empty = []
  ;; run_test "list_of_set empty" test

  let test(): bool = 
    list_of_set (add 5 empty) = [5]
  ;; run_test "list of set singleton" test
  
  let test(): bool = 
    let s = add 5 (add 2 (add 5 (add 1 empty))) in
    (list_of_set s = [1; 2; 5])
  ;; run_test "list of set multiple elements " test

  (*testing the add func*)

  let test(): bool = 
    add 5 empty = set_of_list [5]
  ;; run_test "add one element" test 
  

  
  let test(): bool = 
    list_of_set (add 4 (add 3 empty)) = [3;4]
  ;;run_test "adding out of order" test
  
  ;;print_endline 
  let test(): bool = 
    list_of_set(add 3 (add 2 (add 4 (add 3 empty)))) = [2; 3; 4]
  ;; run_test "add multiple elements out of order with duplicates" test 
  
  (*testing the remove func*)
  let test(): bool = 
    list_of_set(remove 4 empty) = []
  ;; run_test "remove element from empty test" test

  let test(): bool = 
    list_of_set (remove 1 (add 1 empty)) = []
  ;; run_test "remove only element in set" test

  let test(): bool = 
    let s = add 3 (add 2 (add 4 (add 3 empty))) in 
    list_of_set(remove 1 s) = list_of_set s
  ;; run_test "remove element thats not in the set" test 
  
  let test(): bool = 
    let s = add 3 (add 2 (add 4 (add 3 empty))) in 
    list_of_set (remove 3 s) = list_of_set (add 2 (add 4 empty))
  ;; run_test "remove element in set with mulitple elements" test 

  (*testing the member func*)
  let test(): bool = 
    not (member 1 empty)
  ;; run_test "member element in empty set" test

  let test(): bool = 
    let s = set_of_list [1] in
    (member 1 s)
  ;; run_test "member element in singleton" test

  let test(): bool = 
    let s = set_of_list [1] in
    (member 1 s)
  ;; run_test "member element in singleton" test 

  let test(): bool = 
    let s = set_of_list [4; 5; 2; 3] in
    (member 4 s)
  ;; run_test "member element in set w multiple elements" test 

  let test(): bool = 
    let s = set_of_list [4; 5; 2; 3] in
    not (member 6 s)
  ;; run_test "member element that isn't in set w multiple elements" test

  (*testing the size func*)
  let test(): bool = 
    size empty = 0
  ;; run_test "size empty set" test 

  let test(): bool = 
    size (add 1 empty) = 1
  ;; run_test "size singleton" test

  let test(): bool = 
    let l = [4; 3; 2; 1] in
    size (set_of_list l) = 4
  ;; run_test "size multiple elements in set" test

  (*testing the equals func*)
  let test(): bool = 
    equals empty (set_of_list [])
  ;; run_test "equals two empty sets" test 

  let test(): bool = 
    not (equals empty (set_of_list [2; 3]))
  ;; run_test "equals one empty set, one not empty set" test

  let test(): bool = 
    not (equals empty (set_of_list [2; 3]))
  ;; run_test "equals one empty set, one not empty set" test

  let test(): bool = 
    let s1 = set_of_list [2; 3; 1; 2] in 
    let s2 = add 2 (add 2 (add 3 (add 1 empty))) in
    (equals s1 (s2))
  ;; run_test "equals two equal sets w multiple elements" test

  let test(): bool = 
    let s1 = set_of_list [2; 4; 1; 2] in 
    let s2 = add 2 (add 2 (add 3 (add 1 empty))) in
    not (equals s1 (s2))
  ;; run_test "equals two sets with same size but one element different" test

  let test(): bool = 
    let s1 = set_of_list [1] in 
    let s2 = (add 1 empty) in
    (equals s1 (s2))
  ;; run_test "equals two equal singleton sets" test

  let test(): bool = 
    let s1 = set_of_list [2; 3; 1; 2] in 
    let s2 = add 2 (add 2 (add 1 empty)) in
    not (equals s1 s2) 
  ;; run_test "equals sets with different sizes" test

  (*testing the set_of_list func*)
  let test(): bool = 
    equals (set_of_list []) empty
  ;; run_test "set_of_list empty list" test

  let test(): bool = 
    equals (set_of_list [1]) (add 1 empty)
  ;; run_test "set_of_list singleton" test

  let test(): bool = 
    let l = set_of_list [4; 3; 2; 1; 2] in 
    let s = add 4 (add 3 (add 2 (add 1 empty))) in
    equals l s
  ;; run_test "set_of_list list w duplicates and not in order" test

  (* ---------- Write your own test cases above. ---------- *)

end

(* The rest of the file instantiates the above tests so they are
   executed for both OrderedListSet and BSTSet.  Don't modify anything
   below this comment. *)

module TestOrderedListSet = SetTest(ListSet.OrderedListSet)
;; print_newline ()

module TestBSTSet = SetTest(TreeSet.BSTSet)
;; print_newline ()
