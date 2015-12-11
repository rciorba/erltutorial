PROJECT = erltutorial
DEPS = cowboy jiffy hackney # erlastic_search jsx
# dep_erlastic_search = git https://github.com/tsloughter/erlastic_search master
dep_hackney = git http://github.com/benoitc/hackney master
# dep_jsx = git https://github.com/talentdeficit/jsx master
include erlang.mk
