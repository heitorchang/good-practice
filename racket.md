The Racket language does not automatically provide (check-expect a b)

Must (require test-engine/racket-tests)

then write the desired (check-expect) forms

then run them with (test)