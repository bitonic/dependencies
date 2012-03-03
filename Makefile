
SOURCES=$(wildcard *.erl)
TARGETS=$(patsubst %.erl, %.beam, $(SOURCES))
BASIC_PLT=basic.plt

all: $(TARGETS)

dialyze: $(BEAM_TARGETS) $(BASIC_PLT)
	dialyzer --plt basic.plt --no_native --fullpath -Wrace_conditions $(TARGETS)

$(BASIC_PLT): $(BEAM_TARGETS)
	if [ -f $@ ]; then \
	    touch $@; \
	else \
	    dialyzer --output_plt $@ --build_plt --apps stdlib; \
	fi

ERLC_OPTS= -Wall -v +debug_info

%.beam: %.erl
	erlc $(ERLC_OPTS) $<

clean:
	rm -f *.beam
