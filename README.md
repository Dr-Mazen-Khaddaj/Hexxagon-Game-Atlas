# Hexxagon Game on Cardano

Welcome to the Hexxagon Game on Cardano, a decentralized adaptation of the classic Hexxagon game leveraging the Cardano blockchain. Players can lock in cryptocurrency (ADA and any token on the Cardano blockchain), with the winner taking the total sum. This game utilizes the high-security features of the Cardano blockchain to offer a secure and deterministic gaming experience.

## Features

- Play Hexxagon on the Cardano blockchain.
- Lock in ADA or any Cardano-based token as a bet.
- High-security level and deterministic gameplay thanks to Cardano's blockchain technology.

## Setup and Installation

### Prerequisites

- Ensure you have Nix installed if you plan to compile the source code.
- A Linux operating system.

### Installation Steps

**1**. Clone the repository:
   ```
   git clone https://github.com/Dr-Mazen-Khaddaj/Hexxagon-Game-Atlas.git
   ```

**2**. Set up the configuration file:
- Create a `Configurations` directory.
- Inside `Configurations`, create a `config.json` file with the following content:
  ```json
  {
    "coreProvider": { "maestroToken": "<Your-API-Key>" },
    "networkId": "testnet-preview",
    "logging": [{ "type": { "tag": "stderr" }, "severity": "Error", "verbosity": "V3" }],
    "utxoCacheEnable": false
  }
  ```
- Replace `<Your-API-Key>` with a valid Maestro API key.

**3**. Running the Game:
- To run the game using the precompiled executable:
    ```bash
    unzip Hexxagon-Game.zip
    ./Hexxagon-Game
    ```
- Alternatively, compile the code:
  - Enter the repository and execute `nix develop` to create the Nix environment (this may take some time).
  - Once the environment is set up, run `cabal run` to compile the game.

## Game Design Document

The smart contracts' design and the overall game design are detailed in the document 'Hexxagon on Cardano - Game Design.pdf' available in the repository. It provides a comprehensive overview of the game mechanics, smart contract structure, and the interaction flow within the game.

## Usage

  Follow the in-game instructions and use the help pages for guidance. You can type `help` in certain situations to view the help page.

## Local Version

For those interested in a non-blockchain version of the Hexxagon game, my colleague [Andrew](https://github.com/ovitus) has developed a local version, which does not use the Cardano blockchain. This version can be found at [this repository](https://github.com/ovitus/hexxagon.git).

## Contact and Support

  For any inquiries or to report issues, please contact mazenkhaddaj@outlook.com.
  Authors: Dr. Mazen Khaddaj, Andrew Garrett Wright

## Disclaimer

  This project is provided as-is without any warranty. Players participate at their own risk.

