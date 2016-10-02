# Parameters with default values
EMACS ?= emacs
CASK ?= cask

# Paths
ELPA_DIR = .elpa
ELPA_INSTALL = $(ELPA_DIR)/install.log

# Build rules
$(ELPA_DIR):; mkdir -p $@
$(ELPA_INSTALL): $(ELPA_DIR); $(CASK) install 2> $@
test: $(ELPA_INSTALL); $(CASK) exec ert-runner
.PHONY: test

# Clean up rules
clean-elpa:; -rm -r $(ELPA_DIR)
clean-elc:; -find . -name '*.elc' -delete
clean: clean-elpa clean-elc
.PHONY: clean-elpa clean-elc clean
