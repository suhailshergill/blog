INSTALL = /usr/bin/install
WHICH = /usr/bin/which

GHCRTS = +RTS -M300m -RTS

prefix = $(root)
bindir = $(prefix)/bin
blogInitd = /etc/init.d/blog

root = $(realpath $(CURDIR)/../..)
logDir = $(root)/logs
stdout = /dev/null
stderr = $(logDir)/stderr.log
logFiles = $(stderr) blog.prof blog.summary

launcherFile= $(root)/bin/blog-launcher
RTSOPTS = $(GHCRTS) +RTS -pa -sblog.summary -xc -RTS
define launcherContent
#! /bin/sh
pwd
exec $(bindir)/blog $(RTSOPTS) $$@ 1> /dev/null 2> $(stderr)
endef
export launcherContent


.PHONY: all
all: blog

.PHONY: blog
blog:
	cabal clean && cabal configure && cabal build --ghc-options='$(GHCRTS)'
	cabal configure -f prof && cabal build --ghc-options='$(GHCRTS)'
	haskdogs -e

.PHONY: install
install: blog blog-launcher
	cabal install
	sudo $(blogInitd) stop
	$(call map,rotateLog,$(logFiles))
	$(INSTALL) `$(WHICH) blog` $(bindir)
	sudo $(blogInitd) start
	@sleep 5
	$(MAKE) error-file

.PHONY: blog-launcher
blog-launcher:
	echo "$$launcherContent" > $(launcherFile)
	chmod +x $(launcherFile)

.PHONY: error-file
error-file:
	curl localhost:53000/foo -o $(CURDIR)/static/error/notFound.html

rotateLog = cp $(1) $(logDir)/$(notdir $(1)).1;
map = $(foreach a,$(2),$(call $(1),$(a)))

.PHONY: setup
setup:
	bundle install
	cabal-meta install --ghc-options='$(GHCRTS)'

