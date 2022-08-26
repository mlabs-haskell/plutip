Plutip has executable 'local-cluster' starting a cluster of 3 pools and a relay node. This cluster communicates on a local private network. Additional `cardano-node`s can connect to this network and participate in the private blockchain.

## Environment prerequisites

The same requirements from the `vasil-hardfork.md` file needs to be meet. Additionally, the machine needs to have `Docker` and `Docker Compose` installed, with the `Docker Daemon` running.

Plutip will require that version `1.35.x` of `cardano-node` and `cardano-cli` need to be in the `PATH`. Docker will download an older version of those executables, separate from the executables that are available on the `PATH`.

## Starting the Local Cluster
In Plutip's root directory, the local-cluster can be started with a single command:

```cabal run local-cluster -- ./temp```

The `local-cluster` executable expects a sinlge argument for Plutip's working directory. In order to match the default settings that Docker will be using, `./temp` should be used as the working directory. An alternative directory can be used, you will just need to one of two options when starting docker.

Wait until the setup for the `local-cluster` has finished, before continuing.

## Connecting Separate Cardano Node
With Plutip's `local-cluster` running, open another terminal in Plutip's root directory.

In order to start the Docker container that runs the `cardano-node`, just execute thefollowing script:

```./start-old-node```

If an alternative directory was used for Plutip's working directory (besides `./temp`), make sure to do one of the following:
* Create an environment variable in your shell named `WORKING_DIR` and assign the same directory path to the variable.
* Add the path to the working directory as an argument to `./start-old-node {WORK_DIR_PATH}`.

By default, the `cardano-node` version in the docker container is `1.34.1`. This can be overridden by creating an environment variable in the shell with the name `CARDANO_NODE_VERSION`.

To test the docker container has properly started and connected, we can run two commands.

To see the version of `cardano-cli` that docker is using:
```docker-compose exec cardano-node cardano-cli --version```

To see that the `cardano-node` inside the docker container is connected to Plutip's network:
```docker-compose exec cardano-node cardano-cli query tip --mainnet```

## Hard-Forking
After the custom tests have been setup on Plutip and Docker, we can use the same scripts in the `cluster-data` folder to hard-fork Plutip's `local-cluster`.

Initiate hard-fork:
   1. Start in Plutip's root directory (`cardano-cli` should be in `PATH`).
   2. Set socket acquired during cluster start (step 2 above)

       ```export CARDANO_NODE_SOCKET_PATH=<socket path>```
   3. submit update-proposal bumping major version to 7 (babbage)

       ```bash cluster-data/update-proposal-major-version.sh```

   4. submit update-proposal updating the cost model (cost model in cluster-data/cost-models-data)

      ```bash cluster-data/update-proposal-cost-model.sh```

      It waits first for new epoch in babbage era.

After the hard-fork, the node inside Docker container will no longer work. To stop the node use this command:

```docker-compose down```