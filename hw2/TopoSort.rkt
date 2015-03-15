; Authors Matt Pleva, Donovon Bacon, Dakota Polenz
; CS152
; 2015-03-11
; Homework 2 - Topological Sort in Scheme

#lang racket ; needed to compile with DrRacket (not sure if necessary)

; Define the graph (in the form of an adjacency list) to test with topological sort
(define graph '((1 2 3 4) (2 4 5) (3 6) (4 3 6 7) (5 4 7) (6) (7 6) (8 3)))

; Creates a list without a given element e, returns list l
(define (createListWithoutElement l e)
  (if (equal? l '()) '() ; check if l is an empty list
    (if (member (car l) e)
      (createListWithoutElement (cdr l) e) ; recursively call function
      (cons (car l) (createListWithoutElement (cdr l) e))
    )
  )
)

; builds sorted list
(define (buildList l)
  (if (equal? l '()) '() ; check if l is an empty list
    (cons (caar l) (buildList (cdr l))) ; recursive call
  )
)

; finds nodes with zero indegree 
(define (findZeroIndegree origList nodeList)
  (if (equal? origList '()) nodeList
    (findZeroIndegree (cdr origList) (createListWithoutElement nodeList (cdr (car origList)))) 
    ; recursively call function and create build list with zero indegree nodes
  )
)

; remove given node from the list
(define (removeNodeFromGraph node graphList)
  (if (equal? graphList '()) '() ; check for empty list
    (if (equal? (caar graphList) node)
      (removeNodeFromGraph node (cdr graphList)) ; if not equal to node, recursively call function
      (cons (car graphList) (removeNodeFromGraph node (cdr graphList)))
    )
  )
)

; define topological sort function
(define (topoSort l)
  (if (equal? l '()) '() ; check for empty list 
    (append (list (car(findZeroIndegree l (buildList l))))
      (topoSort ; recursively call topological sort
        (removeNodeFromGraph
          (if (equal? (findZeroIndegree l (buildList l)) '()) '()
            (car (findZeroIndegree l (buildList l)))
          )
          l ; return sorted list l
        )
      )
    )
  )
)

; call topological sort on given graph and return the answer
(topoSort graph)
