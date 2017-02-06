# Trust store HTTP server.

HTTP server to list CA certificates from a directory.
To use with [RabbitMQ trust store plugin](https://github.com/rabbitmq/rabbitmq-trust-store).
For demonstration purposes.

Will list `.pem` files from the selected directory.

### API:

- `/` - JSON list of certificates in format `{"certificates":[{"id": <id>, "url": <url>}, ...]}
- `/certs/<file_name>` - PEM encoded certificate files

````
<id> = <file_name>:<file_modification_date>
<url> = /certs/<file_name>
<file_name> = name of a PEM file in the listed directory
```

### Usage:

To rebuild and run a release (requires Erlang to be installed):

```
make run CERT_DIR="/my/cacert/directory" PORT=8080
```

To run from the pre-built escript (requires Erlang to be installed):

```
CERT_DIR="/my/cacert/directory" PORT=8080 ./trust_store_http
```


### HTTPS:

To start an HTTPS server, you should provide ssl options. It can be done via
Erlang `.config` file format:

```
[{trust_store_http,
    [{ssl_options, [{cacertfile,"/path/to/testca/cacert.pem"},
                    {certfile,"/path/to/server/cert.pem"},
                    {keyfile,"/path/to/server/key.pem"},
                    {verify,verify_peer},
                    {fail_if_no_peer_cert,false}]}]}]
```


This configuration can be added to `rel/sys.config`
if you're running the application from source `make run`

Or it can be specified as an environment variable:

```
CERT_DIR="/my/cacert/directory" PORT=8443 CONFIG_FILE=my_config.config ./trust_store_http
```

Port and directory can be also set via config file:


```
[{trust_store_http,
    [{directory, "/tmp/certs"},
     {port, 8081},
     {ssl_options, [{cacertfile,"/path/to/testca/cacert.pem"},
                    {certfile,"/path/to/server/cert.pem"},
                    {keyfile,"/path/to/server/key.pem"},
                    {verify,verify_peer},
                    {fail_if_no_peer_cert,false}]}]}]
     }
    }
```
