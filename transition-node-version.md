# Transition Node Versions

This setup provides instructions on how to start a disposable local network which works with different node versions and is capable to execute Vasil hard fork. The goal is to provide an environment where dApp developers can test how dApp behaves with older and newer node versions and check stability during hard fork. This setup when fully started will give access to 2 node sockets: one socket of an older node version that can talk to a stake pool run by newer node version, and second socket - socket of one of the aforementioned stake pool nodes. So dApp developers will be able to switch node versions by changing sockets to interact with.
 
Stake pool is handled by Plutip tool. Older node version can be started with prepared Docker setup or manually, although Docker setup will be easier to start with as it takes care of requred environment (see details below).


Plutip has executable 'local-cluster' starting a cluster of 3 pools and a relay node. This cluster communicates on a local private network. Additional `cardano-node`s can connect to this network and participate in the private blockchain.

To start working with setup clone Plutip repo and check out to correct branch:
```bash
    git clone https://github.com/mlabs-haskell/plutip.git
    cd plutip
    git checkout vasil-local-cluster-network
```

## Environment prerequisites

To start Plutip's local cluster same requirements from the [vasil-hardfork.md](./vasil-hardfork.md) file needs to be meet. Additionally, for setup with Docker the machine needs to have `Docker` and `Docker Compose` installed, with the `Docker Daemon` running.

Plutip will require that version `1.35.x` of `cardano-node` and `cardano-cli` need to be in the `PATH`. Docker will download an older version of those executables, separate from the executables that are available on the `PATH`.

## Starting the Local Cluster
In Plutip's root directory, the local-cluster can be started with a single command:

```
cabal run local-cluster -- ./temp
```

The `local-cluster` executable expects a sinlge argument for Plutip's working directory. In order to match the default settings that Docker will be using, `./temp` should be used as the working directory. An alternative directory can be used, you will just need to one of two options when starting docker.

Wait until the setup for the `local-cluster` has finished, before continuing. Path to node socket will be printed to terminal.

## Connecting Separate Cardano Node
### With Docker

With Plutip's `local-cluster` running, open another terminal in Plutip's root directory.

In order to start the Docker container that runs the `cardano-node`, just execute thefollowing script:

```
./start-old-node
```

If an alternative directory was used for Plutip's working directory (besides `./temp`), make sure to do one of the following:
* Create an environment variable in your shell named `WORKING_DIR` and assign the same directory path to the variable.
* Add the path to the working directory as an argument to `./start-old-node {WORK_DIR_PATH}`.

By default, the `cardano-node` version in the docker container is `1.34.1`. This can be overridden by creating an environment variable in the shell with the name `CARDANO_NODE_VERSION`.

To test the docker container has properly started and connected, we can run two commands.

To see the version of `cardano-cli` that docker is using:
```
docker-compose exec cardano-node cardano-cli --version
```

To see that the `cardano-node` inside the docker container is connected to Plutip's network:
```
docker-compose exec cardano-node cardano-cli query tip --mainnet && cardano-cli query tip --mainnet
```

This command will first print the tip of the blockchain that the `cardano-node` inside of docker sees. Then it will print the tip of the blockchain from the `cardano-node` that Plutip uses. The values of the responses should be identical (might not be in the same order).

Socket of the old node will have path `./temp/old-node/node.socket` (or dir that was passed as an argument to `./start-old-node` instead of `./temp`).

### Manual
Starting a different version of `cardano-node` can be done without the assistance of Docker, however, you do need to keep track of your shell's environment variables.

Plutip will need to have the newest version of `cardano-node` and `cardano-cli` in the `PATH`. Therefore, the older versions cannot be in the `PATH`. However, this  can be mitigated by giving an alias to the older versions inside of the machine's `.bashrc`. Or by simply just running the specific executables directly.

Also, each `cardano-node` will expect different paths for the `node.socket` that is in the environment. Consequently, you will need to manually change the `CARDNANO_NODE_SOCKET_PATH` for each time you would like to use the different `cardano-cli` version to interact with the blockchain.

Similar to the Docker instructions, Plutip's `local-cluster` will need to be started prior to the old `cardano-node` version can connect. The following command can be used if you first navigate to the folder that contains the old `cardano-node` version.

```
./cardano-node run \
--config /PLUTIP/WORKING_DIR/PATH/node/node.config \
--topology /PLUTIP/WORKING_DIR/PATH/node/node.topology \
--database-path db \
--socket-path node.socket
```

You will need use the config file and the topology file that Plutip generated at start-up, therefore, `/PLUTIP/WORKING_DIR/PATH/` should be the same working directory that was given when starting the `local-cluster`.

To interact with the local blockchain with the old `cardano-cli` version, you will need to set the `CARDANO_NODE_SOCKET_PATH` to the location that was used in the previous `cardano-node` command for the socket-path. Use this command:

```
export CARDANO_NODE_SOCKET_PATH=<old-node-socket-path>
```

Then, when you want to move on to the new `cardano-node` version for additional setup or hard-forking, use same command as before but make sure to use the node-socket path that Plutip is using for it's `cardano-node`.

```
export CARDANO_NODE_SOCKET_PATH=/PLUTIP/WORKING_DIR/PATH/node/node.socket
```

Once the node-socket path has been updated, yhou can use the use the `cardano-cli` that is available in the machine's `PATH` to interact with local blockchain.

## Hard-Forking
After the custom tests have been setup on Plutip and Docker, we can use the same scripts in the `cluster-data` folder to hard-fork Plutip's `local-cluster`.

Initiate hard-fork:
   1. Start in Plutip's root directory (`cardano-cli` should be in `PATH`).
   2. Set socket acquired during cluster start (step 2 above)

       ```
       export CARDANO_NODE_SOCKET_PATH=<socket path>
       ```

   3. submit update-proposal bumping major version to 7 (babbage)

       ```
       bash cluster-data/update-proposal-major-version.sh
       ```

   4. submit update-proposal updating the cost model (cost model in cluster-data/cost-models-data)

      ```
      bash cluster-data/update-proposal-cost-model.sh
      ```

      It waits first for new epoch in babbage era.

After the hard-fork, the `cardano-node` inside Docker container will no longer work. To stop the node use this command if you used the docker method:

```
docker-compose down
```