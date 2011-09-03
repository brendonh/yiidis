all:
	./rebar get-deps
	./rebar compile

run: all
	ERL_LIBS=deps erl -pa apps/*/ebin -config dev -s pt_app start

clean:
	./rebar clean


depclean:
	rm -Rf deps/*

release:
	rm -Rf rel/phptask_bak
#	mv rel/phptask rel/phptask_bak
	./rebar generate
	cp -R rel/phptask_bak/etc/* rel/phptask/etc/

appup:
	./rebar generate-appups previous_release=phptask_bak

startrel:
	./rel/phptask/bin/phptask start

stoprel:
	./rel/phptask/bin/phptask stop

test: all
	cd apps/phptask && ERL_LIBS=../../deps ../../rebar ct
