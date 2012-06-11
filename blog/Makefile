INSTALL = /usr/bin/install
WHICH = /usr/bin/which

prefix = /usr/local
bindir = $(prefix)/bin
blogInitd = /etc/init.d/blog

.PHONY: all
all: blog

.PHONY: blog
blog:
	cabal clean && cabal configure && cabal build

.PHONY: install
install: blog
	cabal install
	sudo $(blogInitd) stop
	sudo $(INSTALL) `$(WHICH) blog` $(bindir)
	sudo $(blogInitd) start

