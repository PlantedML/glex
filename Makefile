all: format doc README.md

# Pinned: clang-format output differs between versions
CLANG_FORMAT ?= uvx clang-format@23.1.3
CPP_SOURCES = $(filter-out src/RcppExports.cpp,$(wildcard src/*.cpp src/*.h src/lib/*.cpp src/include/*.hpp))

.PHONY: format
format:
	air format .
	$(CLANG_FORMAT) -i $(CPP_SOURCES)

.PHONY: format-check
format-check:
	air format --check .
	$(CLANG_FORMAT) --dry-run --Werror $(CPP_SOURCES)

.PHONY: doc
doc: format
	Rscript -e "usethis::use_tidy_description()"
	Rscript -e "devtools::document()"

.PHONY: check
check: doc
	Rscript -e "devtools::check()"

.PHONY: build
build: doc
	Rscript -e "devtools::build()"

.PHONY: install
install: doc
	Rscript -e "pak::local_install(upgrade = FALSE)"

.PHONY: deps
deps:
	Rscript -e "pak::local_install_dev_deps()"

.PHONY: test
test:
	Rscript -e "devtools::test(reporter = 'summary')"

.PHONY: coverage
coverage:
	Rscript -e "covr::report(covr::package_coverage(\".\"), file = \"coverage.html\")"

.PHONY: update-workflows
update-workflows:
	Rscript -e "usethis::use_github_action('check-standard')"
	Rscript -e "usethis::use_github_action('pkgdown')"
	Rscript -e "usethis::use_github_action('test-coverage')"

README.md: README.Rmd
	Rscript -e "rmarkdown::render('README.Rmd', output_file = 'README.md')"
	-rm README.html

.PHONY: site
site: doc README.md
	Rscript -e "pkgdown::build_site()"

codemeta.json: DESCRIPTION
	Rscript -e "codemetar::write_codemeta()"

.PHONY: release
release: clean doc site codemeta.json

# make bump BUMP=patch|minor|major|dev
BUMP ?= dev
.PHONY: bump
bump:
	Rscript -e "usethis::use_version('$(BUMP)')"

# make release-issue VERSION=1.2.0
.PHONY: release-issue
release-issue:
	$(if $(VERSION),,$(error VERSION required, e.g. make release-issue VERSION=1.2.0))
	Rscript -e "usethis::use_release_issue('$(VERSION)')"

# After CRAN acceptance: tags vX.Y.Z from DESCRIPTION, NEWS.md section as body
.PHONY: gh-release
gh-release:
	Rscript -e "usethis::use_github_release()"

.PHONY: thanks
thanks:
	Rscript -e "usethis::use_tidy_thanks()"

# Build artifacts only: document() already rewrites NAMESPACE/man, so deleting them just breaks installs if doc fails
.PHONY: clean
clean:
	-rm -f src/*.o src/*.so src/lib/*.o
	-rm -rf README_cache
