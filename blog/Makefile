INSTALL = /usr/bin/install
WHICH = /usr/bin/which

override GHCRTS += +RTS -M300m -RTS
v = 1

prefix = $(root)
bindir = $(prefix)/bin
deploydir = $(HOME)/workspace
blogInitd = /etc/init.d/blog

root = $(realpath $(CURDIR)/../..)
logDir = $(root)/logs
stdout = /dev/null
stderr = $(logDir)/stderr.log
logFiles = $(stderr) blog.prof blog.summary

launcherFile= $(bindir)/blog-launcher
RTSOPTS = $(GHCRTS) +RTS -pa -sblog.summary -xc -RTS
define launcherContent
#!/bin/sh
#pwd
#exec $(bindir)/blog $(RTSOPTS) $$@ 1> /dev/null 2> $(stderr)
exec $(bindir)/blog $$@ 1> /dev/null 2> $(stderr)
endef
export launcherContent


.PHONY: all
all: blog

.PHONY: blog
blog: build doc

.PHONY: build
build: build-prof

.PHONY: build-reg
build-reg: clean
	cabal configure
	cabal build -v$(v) --ghc-options='$(GHCRTS)'

.PHONY: build-prof
build-prof: build-reg
	cabal configure -f prof
	cabal build -v$(v) --ghc-options='$(GHCRTS)'

.PHONY: clean
clean:
	-rm -rf dist/
	cabal clean

.PHONY: doc
doc:
	#haskdogs -e

.PHONY: deploy
deployBin = $(INSTALL) `$(WHICH) $(1)` $(deploydir); upx $(deploydir)/$(1);
deploy: blog
	cabal install
	$(call map,deployBin,blog publish-entry)

.PHONY: install
installBin = upx -d `$(WHICH) $(1)`; $(INSTALL) `$(WHICH) $(1)` $(bindir);
install: blog-launcher css
	sudo $(blogInitd) stop
	$(call map,rotateLog,$(logFiles))
	$(call map,installBin,blog publish-entry)
	sudo $(blogInitd) start
	@sleep 5
	$(MAKE) error-file

.PHONY: css
css:
	bundle exec compass compile compass

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
setup: setup-ruby setup-haskell

.PHONY: setup-ruby
setup-ruby:
	bundle install

.PHONY: setup-haskell
setup-haskell:
	cd ..; cabal-meta install -v$(v) --ghc-options='$(GHCRTS)'
	cabal-meta install -v$(v) --only-dependencies --ghc-options='$(GHCRTS)'

.PHONY: travis
travis:
	cd ../travis; cabal-meta install -v$(v) --ghc-options='$(GHCRTS)'
