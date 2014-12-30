PROJECT = wuphf

# dependencies

DEPS = json_stringify simple_env gun \
  cowboy gen_cowboy cowboy_base cowboy_cors cowboy_compiled_router cowboy_dev_logger

dep_cowboy = git https://github.com/ninenines/cowboy 1.0.1
dep_gen_cowboy = git https://github.com/camshaft/gen_cowboy.git
dep_cowboy_base = git https://github.com/camshaft/cowboy_base.git
dep_cowboy_cors = git https://github.com/camshaft/cowboy_cors.git
dep_cowboy_empty_favicon = git https://github.com/camshaft/cowboy_empty_favicon.git
dep_cowboy_env = git https://github.com/camshaft/cowboy_env.git
dep_cowboy_compiled_router = git https://github.com/camshaft/cowboy_compiled_router.git
dep_cowboy_livereload = git https://github.com/camshaft/cowboy_livereload.git
dep_cowboy_json_bodyparser = git https://github.com/camshaft/cowboy_json_bodyparser.git
dep_cowboy_dev_logger = git https://github.com/camshaft/cowboy_dev_logger.git

dep_gun = git https://github.com/extend/gun master
dep_json_stringify = git https://github.com/camshaft/json_stringify.git

SHELL_DEPS = rl

dep_rl = git https://github.com/camshaft/rl.git

include erlang.mk

repl: all bin/start
	@bin/start wuphf

bin/start:
	@mkdir -p bin
	@curl https://gist.githubusercontent.com/camshaft/372cc332241ac95ae335/raw/start -o $@
	@chmod a+x $@

.PHONY: repl
