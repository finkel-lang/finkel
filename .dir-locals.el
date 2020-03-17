;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (fill-column . 80)
  (indent-tabs-mode . nil))

 (finkel-mode
  (eval finkel-put-indent-method 'define-macro 'finkel-indent-multiargs)
  (eval finkel-put-doc-string-elt 'define-macro 2)

  (eval finkel-put-indent-method 'define-macro\' 'finkel-indent-multiargs)
  (eval finkel-put-doc-string-elt 'define-macro\' 2)

  (eval finkel-put-indent-method 'describe 1)
  (eval finkel-put-indent-method 'it 1)))
