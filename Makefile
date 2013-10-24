EMACS=emacs

.PHONY: show-version test

show-version:
	@ echo "### Emacs Information ###"
	@ echo "PATH = `which ${EMACS}`"
	${EMACS} --version

test: show-version
	@ echo "### elixir-mix test suite ###"
	${EMACS} -batch -Q -l test/test-runner.el
