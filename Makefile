# prepare the package for release

DELETE  := rm -rf
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: install

clean:
	$(DELETE) src/*.o src/*.so

document: 
	Rscript -e 'devtools::document(roclets = c("rd", "collate", "namespace"))'

compile-attributes: 
	Rscript -e 'Rcpp::compileAttributes()'

build: document compile-attributes
	cd ..;\
	R CMD build --no-manual $(PKGSRC)

build-cran: compile-attributes
	cd ..;\
	R CMD build $(PKGSRC)

install: compile-attributes
	cd ..;\
	R CMD INSTALL --no-multiarch --with-keep.source $(PKGNAME)

clean-install: compile-attributes
	R CMD INSTALL --preclean --no-multiarch --with-keep.source $(PKGNAME)

check: compile-attributes
	Rscript -e 'devtools::check()'

test: compile-attributes
	Rscript -e 'devtools::test()'

vignettes:
	Rscript -e 'devtools::build_vignettes()'


