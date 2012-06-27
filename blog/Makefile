INSTALL = /usr/bin/install
WHICH = /usr/bin/which

prefix = $(root)
bindir = $(prefix)/bin
blogInitd = /etc/init.d/blog

root = $(realpath $(CURDIR)/../..)
logDir = $(root)/logs
stdout = /dev/null
stderr = $(logDir)/stderr.log
logFiles = $(stderr) blog.prof blog.summary

launcherFile= $(root)/bin/blog-launcher
RTSOPTS = +RTS -pa -sblog.summary -RTS
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
	cabal clean && cabal configure && cabal build
	cabal configure -f prof && cabal build

.PHONY: install
install: blog blog-launcher
	cabal install
	sudo $(blogInitd) stop
	$(call map,rotateLog,$(logFiles))
	$(INSTALL) `$(WHICH) blog` $(bindir)
	sudo $(blogInitd) start
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
