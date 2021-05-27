#lang rosette

(provide (all-defined-out))

; Takes as input a propositional formula and returns
; * 'TAUTOLOGY if every interpretation I satisfies F;
; * 'CONTRADICTION if no interpretation I satisfies F;
; * 'CONTINGENCY if there are two interpretations I and I′ such that I satisfies F and I' does not.
(define (classify F)
  (match (solve (assert F))
    [(? unsat?) "CONTRADICTION"]
    [_ (match (verify (assert F))
         [(? unsat?) "TAUTOLOGY"]
         [_ "CONTINGENCY"])]))

; If solve is not unsat = taut or contingent
; If verify not unsat = contradiction or contingent
; If both, then contingent
; If only solve then taut
; if only verify, then contradiction


(define-symbolic* p q r boolean?)

; (p → (q → r)) → (¬r → (¬q → ¬p))
(define f0 (=> (=> p (=> q r)) (=> (! r) (=> (! q) (! p)))))

; (p ∧ q) → (p → q)
(define f1 (=> (&& p q) (=> p q)))

; (p ↔ q) ∧ (q → r) ∧ ¬(¬r → ¬p)
(define f2 (&& (<=> p q) (=> q r) (! (=> (! r) (! q)))))

(solve (assert f0))
(verify (assert f0))
(classify f0)

(solve (assert f1))
(verify (assert f1))
(classify f1)

(solve (assert f2))
(verify (assert f2))
(classify f2)