# Parameters with default values
EMACS ?= emacs
CASK ?= cask

# Paths
ELPA_DIR = .elpa
ELPA_INSTALL = $(ELPA_DIR)/install.log

# Build rules
all: eosd-pixbuf.so
$(ELPA_DIR):; mkdir -p $@
$(ELPA_INSTALL): $(ELPA_DIR); $(CASK) install 2> $@
test: $(ELPA_INSTALL); $(CASK) exec ert-runner
.PHONY: test

EMACS_ROOT ?= $(HOME)/src/git.savannah.gnu.org/git/emacs
CPPFLAGS = -I$(EMACS_ROOT)/src
CFLAGS = -ggdb3 -Wall -fPIC $(CPPFLAGS) $(shell pkg-config --cflags gtk+-3.0)
LDFLAGS = $(shell pkg-config --libs gtk+-3.0)

eosd-pixbuf.o: eosd-pixbuf.c; $(CC) $(CFLAGS) -c -o $@ $<
eosd-pixbuf.so: eosd-pixbuf.o; $(LD) -shared $(LDFLAGS) -o $@ $^

# Clean up rules
clean-module:; -rm eosd-pixbuf.so eosd-pixbuf.o
clean-elpa:; -rm -r $(ELPA_DIR)
clean-elc:; -find . -name '*.elc' -delete
clean: clean-module clean-elpa clean-elc
.PHONY: clean-elpa clean-elc clean
