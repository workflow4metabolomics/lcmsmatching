TOOL_XML=lcmsmatching.xml
GALAXY_BRANCH=release_16.01
CONDA_DIR=$(HOME)/w4mcnd
PLANEMO_DIR=$(HOME)/.planemo
PLANEMO_VENV=$(HOME)/venv_planemo
SOURCE_VENV=source $(PLANEMO_VENV)/bin/activate

all:

test:
	bash-testthat/testthat.sh test/test-searchmz

planemo-lint: $(PLANEMO_VENV)
	$(SOURCE_VENV) && planemo lint --no_xsd $(TOOL_XML)

planemo-test: $(CONDA_DIR)
	$(SOURCE_VENV) && planemo conda_install --conda_prefix $(CONDA_DIR) $(TOOL_XML)
	$(SOURCE_VENV) && planemo test --conda_prefix $(CONDA_DIR) --galaxy_branch $(GALAXY_BRANCH) --conda_dependency_resolution $(TOOL_XML)

planemo-testtoolshed-diff: dist $(PLANEMO_VENV)
	$(SOURCE_VENV) && cd $< && planemo shed_diff --shed_target testtoolshed

planemo-testtoolshed-update: dist $(PLANEMO_VENV)
	$(SOURCE_VENV) && cd $< && planemo shed_update --check_diff --shed_target testtoolshed

planemo-toolshed-diff: dist $(PLANEMO_VENV)
	$(SOURCE_VENV) && cd $< && planemo shed_diff --shed_target toolshed

planemo-toolshed-update: dist $(PLANEMO_VENV)
	$(SOURCE_VENV) && cd $< && planemo shed_update --check_diff --shed_target toolshed

dist:
	$(RM) -rf dist
	mkdir $@
	cp -R .shed.yml *.R search-mz *.py *.xml README.* test-data $@/

$(CONDA_DIR): $(PLANEMO_VENV)
	$(SOURCE_VENV) && planemo conda_init --conda_prefix $(CONDA_DIR)

$(PLANEMO_VENV):
	virtualenv $@
	$(SOURCE_VENV) && pip install --upgrade pip
	$(SOURCE_VENV) && pip install planemo

clean:
	$(RM) -rf dist $(CONDA_DIR) $(PLANEMO_DIR) $(PLANEMO_VENV)

.PHONY: all clean test dist planemo-lint planemo-test planemon-install planemo-toolshed-diff planemo-toolshed-update planemo-testtoolshed-diff planemo-testtoolshed-update
