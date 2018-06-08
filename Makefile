PROJECT = trust_store_http
PROJECT_DESCRIPTION = Trust store HTTP server
PROJECT_VERSION = 0.1.0

DEPS = cowboy jsx lager
LOCAL_DEPS = ssl
dep_cowboy = hex 2.0.0

DEP_PLUGINS = cowboy

include erlang.mk
