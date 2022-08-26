#!/usr/bin/env bash

# The script argument can be the path to Plutip's working directory. If no 
# argument is provided, then either the environment variable or a default path 
# is used.
if [ -z "$1" ]
  then
    PLUTIP="${WORKING_DIR:-./temp}"
  else
    PLUTIP=$1
    export WORKING_DIR=$1
fi

# This folder will be mounted as a docker volume.
mkdir $PLUTIP/old-node/

# Copy the topology file and the genesis files to the folder visible to docker.
cp $PLUTIP/node/node.topology $PLUTIP/old-node/node.topology
cp $PLUTIP/node/*.json $PLUTIP/old-node/

# Cardano Wallet generates the configuration file while using the absolute paths
# fo the genesis files. Since a docker contianer cannot understand absolute
# paths, make the paths relative to node.config. 
sed 's/^\(AlonzoGenesisFile: \).*$/\AlonzoGenesisFile: alonzo-genesis.json/' \
<(sed 's/^\(ByronGenesisFile: \).*$/\ByronGenesisFile: byron-genesis.json/' \
<(sed 's/^\(ShelleyGenesisFile: \).*$/\ShelleyGenesisFile: shelley-genesis.json/' $PLUTIP/node/node.config)) \
> $PLUTIP/old-node/node.config

# Spin up the cardano-node.
docker-compose up -d