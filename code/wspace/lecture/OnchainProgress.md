# 🧩 Onchain Progress Credential (OPC)

A **Plutus smart contract** for minting, managing, and revoking **on-chain progress credentials** as native Cardano tokens.
Each credential represents a learner’s progress (e.g., completion percentage in a course) and is **issued and controlled by a single authority (issuer)**.

---

## 📚 Table of Contents

1. [Overview](#-overview)
2. [Concept](#-concept)
3. [Architecture](#-architecture)
4. [Smart Contract Components](#-smart-contract-components)

   * [1. On-chain Policy](#1-on-chain-policy)
   * [2. Off-chain Endpoints](#2-off-chain-endpoints)
5. [Setup Instructions](#-setup-instructions)
6. [Running the Contract in Plutus Playground](#-running-the-contract-in-plutus-playground)
7. [Example Workflow](#-example-workflow)
8. [Code Walkthrough](#-code-walkthrough)
9. [Extending the Contract](#-extending-the-contract)
10. [Security Considerations](#-security-considerations)
11. [Glossary](#-glossary)

---

## 🪙 Overview

The **Onchain Progress Credential (OPC)** contract allows an authorized issuer to:

* **Mint** a credential token that encodes a student’s course progress.
* **Revoke** that credential later if necessary (by burning it).

Each credential is represented as a **native Cardano token (NFT-like)**, with the **token name encoding both the credential ID and progress percentage**.

---

## 💡 Concept

Imagine an online learning platform where students earn blockchain-based credentials as they progress through courses.
For example:

* When a student reaches **60% progress**, the issuer mints a token named
  `blockchain-course-001|p=60`.
* When the student completes the course, the issuer mints
  `blockchain-course-001|p=100`.
* If the credential needs to be revoked, the issuer burns it.

Only the **authorized issuer (PubKeyHash)** can mint or burn these tokens.

---

## 🧱 Architecture

```
+-----------------------------------+
|         Onchain Policy            |
|-----------------------------------|
| - Validates mint/burn actions     |
| - Checks issuer signature         |
| - Restricts token quantity (±1)   |
+-----------------------------------+

+-----------------------------------+
|        Offchain Endpoints         |
|-----------------------------------|
| 1. Issue Credential               |
|    - Creates & sends token        |
|    - Logs progress + metadata     |
|                                   |
| 2. Revoke Credential              |
|    - Burns specific credential    |
|                                   |
+-----------------------------------+
```

---

## ⚙️ Smart Contract Components

### 1. **On-chain Policy**

The **minting policy** enforces strict minting and burning rules:

* Only the **issuer’s signature** can authorize transactions.
* Only **one token** can be minted or burned per transaction.

```haskell
mkPolicy :: OPCParams -> OPCAction -> ScriptContext -> Bool
```

Validates:

* ✅ Transaction is signed by the issuer.
* ✅ Exactly one token minted (amount = 1).
* ✅ Exactly one token burned (amount = -1).

---

### 2. **Off-chain Endpoints**

The contract exposes two endpoints:

#### **a. `issue` endpoint**

Mints a new credential token for a learner.

```haskell
Endpoint "issue" IssueParams
```

Parameters (`IssueParams`):

* `ipRecipient`: recipient wallet (PaymentPubKeyHash)
* `ipCredentialId`: credential ID (e.g., “course-xyz-001”)
* `ipProgress`: progress percentage (0–100)
* `ipMetadataURI`: optional IPFS or HTTP link with metadata

#### **b. `revoke` endpoint**

Burns a credential token.

```haskell
Endpoint "revoke" RevokeParams
```

Parameters (`RevokeParams`):

* `rpCredentialName`: name of the credential token to revoke

---

## 🧰 Setup Instructions

### 🪄 Requirements

* GHC (Glasgow Haskell Compiler)
* Cabal
* Plutus SDK (`plutus-apps` or `plutus-playground`)
* Nix (if using the IOHK dev environment)

### ⚡ Steps

1. **Clone your Plutus environment:**

   ```bash
   git clone https://github.com/input-output-hk/plutus-apps.git
   cd plutus-apps
   nix-shell
   ```

2. **Create a new project directory:**

   ```bash
   mkdir onchain-progress-credential
   cd onchain-progress-credential
   ```

3. **Save the contract file:**

   ```bash
   nano OnchainProgressCredential.hs
   ```

   Paste the smart contract code into this file.

4. **Build the project:**

   ```bash
   cabal build
   ```

---

## 🤪 Running the Contract in Plutus Playground

1. Open **[Plutus Playground](https://playground.plutus.iohkdev.io/)**.

2. Paste the contents of `OnchainProgressCredential.hs` into the editor.

3. Compile the contract.

4. In **Simulator**:

   * Choose an **issuer wallet**.
   * Deploy the contract.

5. Call the **`issue`** endpoint:

   ```json
   {
     "ipRecipient": "wallet2PubKeyHash",
     "ipCredentialId": "blockchain-course-001",
     "ipProgress": 75,
     "ipMetadataURI": "ipfs://QmExample"
   }
   ```

   Result → Token `blockchain-course-001|p=75` is minted to `wallet2`.

6. Call the **`revoke`** endpoint:

   ```json
   {
     "rpCredentialName": "blockchain-course-001|p=75"
   }
   ```

   Result → Token is burned by the issuer.

---

## 🧬 Example Workflow

| Step | Actor   | Action                                    | Result            |
| ---- | ------- | ----------------------------------------- | ----------------- |
| 1    | Issuer  | Deploys contract with their `PubKeyHash`  | Policy created    |
| 2    | Issuer  | Calls `issue` with recipient and progress | Token minted      |
| 3    | Student | Receives credential token                 | Proof of progress |
| 4    | Issuer  | Calls `revoke` with token name            | Token burned      |

---

## 🧬 Code Walkthrough

| Section                       | Description                                                  |
| ----------------------------- | ------------------------------------------------------------ |
| `OPCParams`                   | Defines who the issuer is (PubKeyHash)                       |
| `OPCAction`                   | Represents action type (`MintCredential` / `BurnCredential`) |
| `mkPolicy`                    | Core minting logic                                           |
| `policy`                      | Wraps `mkPolicy` into a usable Plutus script                 |
| `IssueParams`, `RevokeParams` | Off-chain request data structures                            |
| `mkTokenName`                 | Encodes credential ID and progress into token name           |
| `issue`                       | Endpoint to mint credential                                  |
| `revoke`                      | Endpoint to burn credential                                  |
| `endpoints`                   | Combines and exposes the two contract endpoints              |

---

## 🧬 Extending the Contract

You can extend the contract by:

* Adding **metadata verification** (CIP-68 / CIP-25 compliance).
* Creating **soulbound credentials** (non-transferable tokens).
* Allowing **delegated issuers** (multiple authorized signers).
* Adding **time locks** (valid minting window).

---

## 🛡️ Security Considerations

* Only the **issuer’s PubKeyHash** can mint or burn tokens.
* Always verify **metadata integrity** off-chain.
* Deploy and test extensively on **Cardano Testnet** before mainnet use.
* Use **multi-sig** or **DAO-controlled minting** for decentralized issuance.

---

## 📖 Glossary

| Term                 | Meaning                                                         |
| -------------------- | --------------------------------------------------------------- |
| **Plutus**           | Cardano’s Haskell-based smart contract language                 |
| **PubKeyHash (PKH)** | Hash of a wallet’s public key, used for signature verification  |
| **Minting Policy**   | On-chain code that defines rules for token creation/destruction |
| **Redeemer**         | Data provided with a transaction to select contract logic       |
| **CurrencySymbol**   | Unique identifier for a native token policy                     |
| **TokenName**        | Text label for a specific token under a currency symbol         |
| **Endpoint**         | Off-chain contract function callable by users                   |
| **Singleton**        | Function to define value with a specific token and quantity     |
| **CIP-25 / CIP-68**  | Cardano Improvement Proposals defining token metadata standards |

---

## 🏁 Summary

The **Onchain Progress Credential** contract enables blockchain-based, verifiable credentials that reflect real progress and achievement — issued and managed entirely on-chain, under issuer control.

This serves as a foundation for **education, certification, or gamified learning systems** powered by **Cardano smart contracts**.
