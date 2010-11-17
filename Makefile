export CC=$(shell icu-config --cc)
export ICU_CFLAGS=$(shell icu-config --cppflags-searchpath) \
                  $(shell icu-config --cflags)
export ICU_LDFLAGS=$(shell icu-config --ldflags) \
                   $(shell icu-config --ldflags-icuio)

.PHONY: clean test doc

all:
	./rebar compile

clean:
	./rebar clean

test:
	./rebar eunit

doc:
	./rebar doc
