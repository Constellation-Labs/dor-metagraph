Metagraph
=========

This directory encompasses all aspects of the metagraph and is built on top of the Data API provided by Constellation Network.

Here, you will find the entire project structure, including validations, state building, testing, and custom endpoints.

### Dependencies

-   Java 11
-   Scala 2

State and CalculatedState
===========================

At Metagraphs, we distinguish between two types of states: `State` and `CalculatedState`.

In the DOR Metagraph, the `State` holds all information pertaining to check-ins in the current snapshot, including:

-   `publicId`
-   `signature`
-   `dts`
-   `dtmCheckInHash`
-   `maybeDorAPIResponse`

On the other hand, the `CalculatedState` contains metadata information about the devices. In contrast to only storing check-ins from the current snapshot, the CalculatedState maintains a map that establishes the relationship between all devices and their metadata. This metadata includes:

-   `devices: Map[Address, DeviceInfo]`
    -   where `Address` represents the device address, and
    -   `DeviceInfo` includes:
        -   `lastCheckIn`
        -   `dorAPIResponse`
        -   `nextEpochProgressToReward`

Code
--------

The main code for this example is located in the following directories:

-   `modules/l0/src/main/scala/com/my/currency/l0`
-   `modules/l1/src/main/scala/com/my/currency/l1`
-   `modules/data_l1/src/main/scala/com/my/currency/data_l1`
-   `modules/shared_data/src/main/scala/com/my/currency/shared_data`

### Application Lifecycle

The methods of the DataApplication are invoked in the following sequence:

-   `validateUpdate`
-   `validateData`
-   `combine`
-   `dataEncoder`
-   `dataDecoder`
-   `calculatedStateEncoder`
-   `signedDataEntityDecoder`
-   `serializeBlock`
-   `deserializeBlock`
-   `serializeState`
-   `deserializeState`
-   `serializeUpdate`
-   `deserializeUpdate`
-   `setCalculatedState`
-   `getCalculatedState`
-   `hashCalculatedState`
-   `routes`

For a more detailed understanding, please refer to the [complete documentation](https://docs.constellationnetwork.io/sdk/frameworks/currency/data-api) on the Data API.

### Lifecycle Functions

#### -> `validateUpdate`

* This method initiates the initial validation of updates on the L1 layer. Due to a lack of contextual information (state), its validation capabilities are constrained. Any errors arising from this method result in a 500 response from the `/data` POST endpoint.

#### -> `validateData`

* This method validates data on the L0 layer, with access to contextual information, including the current state. In this example, we ensure that the provided address matches the one that signed the message. Additionally, we verify the most recent update timestamp to prevent the acceptance of outdated or duplicated data.

#### -> `combine`

* This method takes validated data and the prior state, combining them to produce the new state. In this instance, we update device information in the state based on the validated update.

#### -> `dataEncoder` and `dataDecoder`

* These are the encoder/decoder components used for incoming updates.

#### -> `calculatedStateEncoder`

* This encoder is employed for the calculatedState.

#### -> `signedDataEntityDecoder`

* This function handles the parsing of request body formats (JSON, string, xml) into a `Signed[Update]` class. In this case, we receive a string and parse it into `Signed[CheckInUpdate]`.

#### -> `serializeBlock` and `deserializeBlock`

* The serialize function accepts the block object and converts it into a byte array for storage within the snapshot. The deserialize function is responsible for deserializing into Blocks.

#### -> `serializeState` and `deserializeState`

* The serialize function accepts the state object and converts it into a byte array for storage within the snapshot. The deserialize function is responsible for deserializing into State.

#### -> `serializeUpdate` and `deserializeUpdate`

* The serialize function accepts the update object and converts it into a byte array for storage within the snapshot. The deserialize function is responsible for deserializing into Updates.

#### -> `setCalculatedState`

* This function sets the calculatedState. You can store this as a variable in memory or use external services such as databases. In this example, we use in-memory storage.

#### -> `getCalculatedState`

* This function retrieves the calculated state.

#### -> `hashCalculatedState`

* This function creates a hash of the calculatedState to be validated when rebuilding this state, in case of restarting the metagraph.

#### -> `routes`

Customizes routes for our application.

In this example, the following endpoints are implemented:

-   GET `/data-application/calculated-state/latest`: Returns the latest calculatedState.