export BIODB_CACHE_DIRECTORY=$(CURDIR)/lcms.biodb.cache
CONDA_PREFIX=$(HOME)/.plncnd

all:

test:
	$(MAKE) -C $@

test.travis.install:
	R -e "install.packages(c('getopt', 'devtools'), dependencies = TRUE, repos = 'https://cloud.r-project.org/')"
	bash install_biodb.sh

planemolint.travis.install:
planemotest.travis.install:
	sudo apt-get upgrade
	python --version
	sudo apt-get install python-virtualenv

planemo-venv/bin/planemo: planemo-venv
	. planemo-venv/bin/activate && pip install "pip>=7"
	. planemo-venv/bin/activate && pip install planemo

planemo-venv:
	virtualenv planemo-venv

planemolint: planemo-venv/bin/planemo
	. planemo-venv/bin/activate && planemo lint --no_xsd

planemotest: R_LIBS_USER=
planemotest: planemo-venv/bin/planemo
	. planemo-venv/bin/activate && planemo test --conda_dependency_resolution --conda_prefix "$(CONDA_PREFIX)" --galaxy_branch release_18.09

planemo-testtoolshed-diff: distplanemo-venv/bin/planemo
	. planemo-venv/bin/activate && cd $< && planemo shed_diff --shed_target testtoolshed

planemo-testtoolshed-update: distplanemo-venv/bin/planemo
	. planemo-venv/bin/activate && cd $< && planemo shed_update --check_diff --shed_target testtoolshed

planemo-toolshed-diff: distplanemo-venv/bin/planemo
	. planemo-venv/bin/activate && cd $< && planemo shed_diff --shed_target toolshed

planemo-toolshed-update: distplanemo-venv/bin/planemo
	. planemo-venv/bin/activate && cd $< && planemo shed_update --check_diff --shed_target toolshed

clean:
	$(MAKE) -C test $@
	$(RM) -r $(BIODB_CACHE_DIRECTORY)
	$(RM) -r planemo-venv
	$(RM) -r planemotest.log
	$(RM) -r $(HOME)/.planemo
	$(RM) -r $(CONDA_PREFIX)

.PHONY: all clean test planemo-lint planemo-test planemon-install planemo-toolshed-diff planemo-toolshed-update planemo-testtoolshed-diff planemo-testtoolshed-update test.travis.install planemolint.travis.install planemotest.travis.install
