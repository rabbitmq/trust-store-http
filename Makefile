PROJECT = trust_store_http
PROJECT_DESCRIPTION = Trust store HTTP server
PROJECT_VERSION = 0.1.0

DEPS = cowboy jsx lager
LOCAL_DEPS = ssl
dep_cowboy_commit = 1.0.4

DEP_PLUGINS = cowboy

include erlang.mk
