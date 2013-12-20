#!/bin/sh
BASEDIR=$PWD
if ! test -d $PWD/deps/rabbitmq-codegen; \
	then git clone git://github.com/rabbitmq/rabbitmq-codegen.git $PWD/deps/rabbitmq-codegen; \
	cd $PWD/deps/rabbitmq-codegen; \
	git checkout -b rabbitmq_v2_8_2; \
	cd $BASEDIR; \
fi
if ! test -d $PWD/deps/rabbitmq-server; \
	then git clone git://github.com/rabbitmq/rabbitmq-server.git $PWD/deps/rabbitmq-server; \
	cd $PWD/deps/rabbitmq-server; \
	git checkout -b rabbitmq_v2_8_2; \
	cd $BASEDIR; \
fi
if ! test -d $PWD/deps/rabbitmq-erlang-client; \
	then git clone git://github.com/rabbitmq/rabbitmq-erlang-client.git $PWD/deps/rabbitmq-erlang-client; \
	cd $PWD/deps/rabbitmq-erlang-client; \
	git checkout -b rabbitmq_v2_8_2; \
	cd $BASEDIR; \
fi
cd $PWD/deps/rabbitmq-erlang-client; make all; cd $BASEDIR;

cp -R $PWD/deps/rabbitmq-erlang-client/dist/rabbit_common-0.0.0 deps/rabbitmq-erlang-client/dist/amqp_client-0.0.0/include/rabbit_common

erl -pa $PWD/deps/rabbitmq-erlang-client/dist/amqp_client-0.0.0/ebin -pz ebin -make

mkdir -p dist
mkdir -p ebin

cp $PWD/ebin/mod_wio_notifier.beam $PWD/dist/
cp $PWD/deps/rabbitmq-erlang-client/dist/amqp_client-0.0.0.ez $PWD/dist/
cp $PWD/deps/rabbitmq-erlang-client/dist/rabbit_common-0.0.0.ez $PWD/dist/

echo "Files needed are now in the dist/ folder. Just drop those into the ejabberd/lib/ejabberd/ebin/ folder"