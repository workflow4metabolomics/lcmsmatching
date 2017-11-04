all:

test:
	bash-testthat/testthat.sh test/test-searchmz

planemo-venv/bin/planemo: planemo-venv
	. planemo-venv/bin/activate && pip install --upgrade pip setuptools
	. planemo-venv/bin/activate && pip install planemo

planemo-venv:
	virtualenv planemo-venv

planemolint: planemo-venv/bin/planemo
	. planemo-venv/bin/activate && planemo lint --no_xsd

planemotest: planemo-venv/bin/planemo
	. planemo-venv/bin/activate && planemo test --conda_dependency_resolution --galaxy_branch release_17.05

planemo-testtoolshed-diff: distplanemo-venv/bin/planemo
	. planemo-venv/bin/activate && cd $< && planemo shed_diff --shed_target testtoolshed

planemo-testtoolshed-update: distplanemo-venv/bin/planemo
	. planemo-venv/bin/activate && cd $< && planemo shed_update --check_diff --shed_target testtoolshed

planemo-toolshed-diff: distplanemo-venv/bin/planemo
	. planemo-venv/bin/activate && cd $< && planemo shed_diff --shed_target toolshed

planemo-toolshed-update: distplanemo-venv/bin/planemo
	. planemo-venv/bin/activate && cd $< && planemo shed_update --check_diff --shed_target toolshed

dist:
	$(RM) -r $@
	mkdir $@
	cp -R .shed.yml *.R search-mz *.py *.xml README.* test-data $@/

clean:
	$(RM) -r dist
	$(RM) -r $(HOME)/.planemo
	$(RM) -r planemo-venv

.PHONY: all clean test dist planemo-lint planemo-test planemon-install planemo-toolshed-diff planemo-toolshed-update planemo-testtoolshed-diff planemo-testtoolshed-update
