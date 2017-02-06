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
make run CERT_DIR="/my/cacert/directory"
```

To run from the pre-built escript (requires Erlang to be installed):

```
CERT_DIR="/my/cacert/directory" ./trust_store_http
```