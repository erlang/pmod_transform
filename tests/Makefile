MODULES = \
	test_pmod.beam \
	alpha.beam \
	beta.beam \
	gamma.beam \
	pmod_basic.beam \
	my_lists.beam \
	static_call.beam \
	fun_in_pmod.beam

EBIN = $(dir $(CURDIR))ebin

.PHONY: tests
tests: $(MODULES)
	erl -pa $(EBIN) -noinput -eval 'eunit:test(test_pmod)' -run erlang halt

%.beam: %.erl
	erlc -pa $(EBIN) -Werror $<
