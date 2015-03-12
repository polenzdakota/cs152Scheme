(define graph '((1 2 3 4) (2 4 5) (3 6) (4 3 6 7) (5 4 7) (6) (7 6)))
(define graph2 graph)

(define (createListWithoutElement l e)
  (if (equal? l '()) '()
    (if (member (car l) e)
      (createListWithoutElement (cdr l) e)
      (cons (car l) (createListWithoutElement (cdr l) e))
    )
  )
)

(define (buildList l)
  (if (equal? l '()) '()
    (cons (caar l) (buildList (cdr l)))
  )
)

(define (findZeroIndegree origList nodeList)
  (if (equal? origList '()) nodeList
    (findZeroIndegree (cdr origList) (createListWithoutElement nodeList (cdr (car origList))))
  )
)

(define (removeNodeFromGraph node graphList)
  (if (equal? graphList '()) '()
    (if (equal? (caar graphList) node) '()
      (cons (car graphList) (removeNodeFromGraph node (cdr graphList)))
    )
  )
)

(define (topoSort l)
  (if (equal? l '()) '()
    (append (findZeroIndegree l (buildList l))
      (topoSort
        (removeNodeFromGraph
          (if (equal? (findZeroIndegree l (buildList l)) '()) '()
            (car (findZeroIndegree l (buildList l)))
          )
          l
        )
      )
    )
  )
)


(findZeroIndegree graph (buildList graph))
(removeNodeFromGraph 1 graph)
(topoSort graph)
