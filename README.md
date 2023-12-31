DOR Metagraph
===============

Welcome to the official repository of DOR Metagraph.

The DOR Metagraph operates on Constellation's global Layer 0 network, known as the Hypergraph. It serves as a decentralized ledger-based platform for the distribution, validation, and incentivization of data generated by the Dor Traffic Miner and upcoming product offerings.

This technology is built using Constellation Network's Hypergraph Transfer Protocol (HGTP) and features the DOR utility token. The Dor Metagraph and its associated token have various applications, including:

-   Establishing a global community of individual "Datapreneurs" dedicated to mining valuable datasets.
-   Providing support for hardware and software tools for real-world data collection.
-   Programmatically managing all fee requirements for Hypergraph network usage.

The key users of this metagraph are Datapreneurs, responsible for collecting valuable data for the network, and node operators, who contribute resources such as bandwidth and processing power to the network.

Project Structure
-----------------

This project consists of two main subdirectories:

### Metagraph

The Metagraph folder contains all the content of the metagraph and is constructed on top of the Data API provided by Constellation Network. For more detailed information about this metagraph, please refer to the README.md file within this folder.

### API

The API subdirectory houses a "mock API" designed to simulate returns from the official DOR API. This mock API serves the purpose of testing returns from the API before integrating them into the metagraph. It was developed using NodeJS with Typescript and Express.