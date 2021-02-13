LISP ?= sbcl

build:
	$(LISP) --eval '(ql:quickload :psa)' \
	        --eval '(asdf:make :psa)' \
	        --eval '(quit)'

clean:
	rm psa
