MAKEFLAGS := --silent
SBCL ?= sbcl
CLI ?= lispfmt
TEST_TMP ?= .test-tmp
SRC ?= $(shell find . -type f -name '*.lisp')
INSTALL_DIR ?= $(HOME)/.local/bin
INSTALL_EXE ?= $(INSTALL_DIR)/$(CLI)

.PHONY: build
build: $(CLI)

.PHONY: test
test: test_lib test_cli

$(CLI): $(SRC)
	LISPFMT_BUILD=1 $(SBCL) --noinform --disable-debugger --script cli.lisp

.PHONY: test_lib
test_lib:
	$(SBCL) --script test.lisp

.PHONY: test_cli
test_cli: $(CLI)
	mkdir -p $(TEST_TMP)
	printf '(one two)\n' | $(SBCL) --script cli.lisp >$(TEST_TMP)/out 2>$(TEST_TMP)/err
	printf '(one two)\n\n' >$(TEST_TMP)/expected
	cmp $(TEST_TMP)/expected $(TEST_TMP)/out
	test ! -s $(TEST_TMP)/err
	printf ')\n' | $(SBCL) --script cli.lisp >$(TEST_TMP)/bad-out 2>$(TEST_TMP)/bad-err; test $$? -ne 0
	test ! -s $(TEST_TMP)/bad-out
	grep -q '^\[$(CLI)\] ' $(TEST_TMP)/bad-err
	$(MAKE) build >/dev/null
	test -x $(CLI)
	./$(CLI) --help >$(TEST_TMP)/args-out 2>$(TEST_TMP)/args-err; test $$? -eq 2
	test ! -s $(TEST_TMP)/args-out
	grep -q '^\[$(CLI)\] arguments are not supported' $(TEST_TMP)/args-err
	printf '# ; one\n' | ./$(CLI) >$(TEST_TMP)/exe-out 2>$(TEST_TMP)/exe-err
	printf '#;one\n\n' >$(TEST_TMP)/exe-expected
	cmp $(TEST_TMP)/exe-expected $(TEST_TMP)/exe-out
	test ! -s $(TEST_TMP)/exe-err
	rm -rf $(TEST_TMP)

.PHONY: clean
clean:
	rm -rf $(TEST_TMP) $(CLI)

.PHONY: install
install: build
	mkdir -p "$(INSTALL_DIR)"
	ln -sf "$(shell realpath $(CLI))" "$(INSTALL_EXE)"
	@echo "Symlinked the $(CLI) executable to: \"$(INSTALL_EXE)\"."
	@echo "Hint: make sure \"$(INSTALL_DIR)\" is in your PATH."

.PHONY: uninstall
uninstall:
	rm -f "$(INSTALL_EXE)"
