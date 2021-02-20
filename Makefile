LISP ?= sbcl

psa: psa.lisp psa.asd
	$(LISP) --eval '(ql:quickload :psa)' \
	        --eval '(asdf:make :psa)' \
	        --eval '(quit)'

clean:
	rm psa

.PHONY: clean
