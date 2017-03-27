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

planemo-testtoolshed-diff: $(PLANEMO_VENV)
	$(SOURCE_VENV) && planemo shed_diff --shed_target testtoolshed

$(CONDA_DIR): $(PLANEMO_VENV)
	$(SOURCE_VENV) && planemo conda_init --conda_prefix $(CONDA_DIR)

$(PLANEMO_VENV):
	virtualenv $@
	$(SOURCE_VENV) && pip install --upgrade pip
	$(SOURCE_VENV) && pip install planemo

clean:
	$(RM) -r $(CONDA_DIR)
	$(RM) -r $(PLANEMO_DIR)
	$(RM) -r $(PLANEMO_VENV)

.PHONY: all clean test planemo-lint planemo-test planemon-install
