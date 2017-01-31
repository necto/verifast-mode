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

(ert-deftest verifast-mode-highlight-sources ()
  "Ensure we highlight known values for source."
  (should (assess-face-at=
           "int x;"
           'verifast-mode
           "int"
           'font-lock-type-face)))
