all: doc test

doc: 
	R --vanilla -q -e "devtools::document()"

test: 
	R --vanilla -q -e "devtools::test()"

install: 
	R --vanilla -q -e "devtools::install()"
