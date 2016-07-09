.PHONY : test

EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .

ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

test: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l test/test-command.el -l test/test-util.el \
		-f ert-run-tests-batch-and-exit

test-util: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l test/test-util.el \
		-f ert-run-tests-batch-and-exit

test-command: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l test/test-command.el \
		-f ert-run-tests-batch-and-exit

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
