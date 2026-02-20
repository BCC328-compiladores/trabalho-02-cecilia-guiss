TESTS ?= test/ex1.sl test/ex2.sl test/ex3.sl test/ex4.sl test/ex5.sl test/ex6.sl test/ex7.sl test/ex8.sl test/ex9.sl test/ex10.sl test/ex11.sl test/closure_complex.sl

# Handle extra arguments as files
ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
$(eval $(ARGS):;@:)

ifneq ($(ARGS),)
TESTS := $(ARGS)
endif

all: build

build:
	cabal build

clean:
	cabal clean



# Colors
BOLD_BLUE := \033[1;34m
BOLD_GREEN := \033[1;32m
RESET := \033[0m

lexer:
	@for f in $(TESTS); do \
		printf "$(BOLD_BLUE)>> Running lexer on $$f$(RESET)\n"; \
		cabal run sl -- --lexer $$f || exit 1; \
	done

parser:
	@for f in $(TESTS); do \
		printf "$(BOLD_BLUE)>> Running parser on $$f$(RESET)\n"; \
		cabal run sl -- --parser $$f || exit 1; \
	done

pretty:
	@for f in $(TESTS); do \
		printf "$(BOLD_BLUE)>> Running pretty on $$f$(RESET)\n"; \
		cabal run sl -- --pretty $$f || exit 1; \
	done

typecheck:
	@for f in $(TESTS); do \
		printf "$(BOLD_BLUE)>> Running typechecker on $$f$(RESET)\n"; \
		cabal run sl -- --typecheck $$f || exit 1; \
	done

closure:
	@for f in $(TESTS); do \
		printf "$(BOLD_BLUE)>> Running closure converter on $$f$(RESET)\n"; \
		cabal run sl -- --closure $$f || exit 1; \
	done

run:
	@for f in $(TESTS); do \
		printf "$(BOLD_BLUE)>> Running interpreter on $$f$(RESET)\n"; \
		cabal run sl -- --run $$f || exit 1; \
	done



test: lexer parser pretty typecheck closure run
	@echo "$(BOLD_GREEN)All tests passed!$(RESET)"
