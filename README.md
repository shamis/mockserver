# mockserver
Erlang mockserver to test rest client side code.

Can configure the responses and type of the request for each path and test it.

## Requirements
* Erlang Insatlled

## Setup
Execute the following command insided the mockserver folder
```sh
./rebar g-d co
```

Once that is done execute
```sh
./start.sh
```
Edit the rules.config file for the tests you require to run

In the Erlang console execute the following command
```erlang
mockserver:start(3000).
```
you can use any port number instead of 3000.
