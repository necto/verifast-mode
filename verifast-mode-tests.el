(require 'pp)
(require 'ert)
(require 'verifast-mode)
(require 'assess)

(ert-deftest verifast-pass ()
  "Just a passing test, to make sure they exist"
  t)

(ert-deftest verifast-mode-indent-lemma ()
  "Ensure we correctly indent simple lemmas."
  (should (assess-indentation=
           'verifast-mode
           ;; before:
           "
    lemma void reverse_cons<t>(t head, list<t> tail)
 requires true;
  ensures reverse(cons(head, tail)) == append(reverse(tail), cons(head, nil));
    {
   reverse_append(reverse(tail), cons(head, nil));
                       }"
           ;; after:
           "
lemma void reverse_cons<t>(t head, list<t> tail)
requires true;
ensures reverse(cons(head, tail)) == append(reverse(tail), cons(head, nil));
{
  reverse_append(reverse(tail), cons(head, nil));
}")))

(ert-deftest verifast-mode-indent-less-sign ()
  "Ensure we indent correctly the code after '<',
 as it seems like an opening angle bracket."
  (should (access-indentatio
           'verifast-mode
           "
lemma void nth_cons()
requires 0 < n;
  ensures true;
"
           ;; after:
           "
lemma void nth_cons()
requires 0 < n;
ensures true;
")))


(ert-deftest verifast-mode-indent-simple-switch ()
  "Ensure we correctly indent simple lemmas."
  (should (assess-indentation=
           'verifast-mode
           ;; before:
           "
fixpoint t get_some<t>(option<t> x) {
switch(x) {
case none: return default_value<t>();
case some(v): return v;
}
}"
           ;; after:
           "
fixpoint t get_some<t>(option<t> x) {
  switch(x) {
    case none: return default_value<t>();
    case some(v): return v;
  }
}")))


(ert-deftest verifast-mode-indent-long-switch ()
  "Ensure we correctly indent simple lemmas."
  (should (assess-indentation=
           'verifast-mode
           ;; before:
           "
lemma void length_0_nil<T>(list<T> lst)
     requires length(lst) == 0;
 ensures lst == nil;
           {
  switch (lst) {
  case nil: return;
 case cons(h, t):
  assert(length(lst) > length(t));
  assert(length(t) == 0);
 return;
}
  }"
           ;; after:
           "
lemma void length_0_nil<T>(list<T> lst)
requires length(lst) == 0;
ensures lst == nil;
{
  switch (lst) {
    case nil: return;
    case cons(h, t):
      assert(length(lst) > length(t));
      assert(length(t) == 0);
      return;
  }
}")))

(ert-deftest verifast-mode-indent-argument-on-a-new-line ()
  "Ensure we correctly indent simple lemmas."
  (should (assess-indentation=
           'verifast-mode
           ;; before:
           "
lemma void nth_cons<T>(int n, list<T> lst,
T head)
requires 0 < n;
ensures nth(n-1, lst) == nth(n, cons(head, lst));
{
}
"
           ;; after:
           "
lemma void nth_cons<T>(int n, list<T> lst,
                       T head)
requires 0 < n;
ensures nth(n-1, lst) == nth(n, cons(head, lst));
{
}
")))

(ert-deftest verifast-mode-indent-requires-multi-line-argument ()
  "Ensure we correctly indent simple lemmas."
  (should (assess-indentation=
           'verifast-mode
           ;; before:
           "
lemma void nth_len_append_cons<T>(list<T> lst, T x)
requires true;
ensures nth(length(lst),
  append(lst, cons(x, nil))) == x;
{
}"
           ;; after:
           "
lemma void nth_len_append_cons<T>(list<T> lst, T x)
requires true;
ensures nth(length(lst),
            append(lst, cons(x, nil))) == x;
{
}")))

(ert-deftest verifast-mode-highlight-sources ()
  "Ensure we highlight known values for source."
  (should (assess-face-at=
           "int x;"
           'verifast-mode
           "int"
           'font-lock-type-face)))
