# 🌟 Staking Rewards Smart Contract Tutorial (Plutus)

## 📚 Table of Contents

1. **🎯 Introduction**
   1.1 🧭 Overview
   1.2 🎓 Learning Objectives
2. **💡 Conceptual Background**
   2.1 💰 What Is Staking?
   2.2 🧠 Why Use Plutus for Staking Contracts?
3. **🏗️ Smart Contract Architecture**
   3.1 🧩 Core Components
   3.2 ⚖️ On-Chain vs Off-Chain Logic
4. **🧮 Code Breakdown**
   4.1 📦 Data Types (Datum & Redeemer)
   4.2 🔒 On-Chain Validator Logic
   4.3 🔁 Off-Chain Endpoints
5. **🚀 Step-by-Step Tutorial**
   5.1 ⚙️ Setting Up the Environment
   5.2 🧱 Compiling and Deploying the Contract
   5.3 💼 Using the Endpoints
6. **🧪 Testing and Simulation**
   6.1 🧍‍♂️ How to Simulate Staking
   6.2 🏁 How to Simulate Reward Withdrawal
7. **🛡️ Security and Design Considerations**
   7.1 ⚠️ Potential Risks
   7.2 🧰 Improvements for Production
8. **📖 Glossary**

---

## 1. 🎯 Introduction

### 1.1 🧭 Overview

The **Staking Rewards Contract** is a simple Plutus smart contract that allows users to lock up ADA (or tokens) as a stake and later withdraw their principal along with rewards. The reward calculation is performed **off-chain** to keep the on-chain validator minimal.

### 1.2 🎓 Learning Objectives

* 🧠 Understand how staking logic can be represented in Plutus.
* 🏗️ Learn how to structure a contract with both on-chain and off-chain components.
* 💡 Deploy and interact with a staking contract using endpoints.

---

## 2. 💡 Conceptual Background

### 2.1 💰 What Is Staking?

Staking is the process of locking funds in a contract to earn rewards over time. It is common in proof-of-stake systems like Cardano, where participants help secure the network and get incentives in return.

### 2.2 🧠 Why Use Plutus for Staking Contracts?

Using Plutus allows developers to create **custom staking rules**, such as time-based rewards, multiple reward tiers, or token-based staking — all while remaining fully decentralized.

---

## 3. 🏗️ Smart Contract Architecture

### 3.1 🧩 Core Components

| 🧱 Component  | 📝 Description                                                                                |
| ------------- | --------------------------------------------------------------------------------------------- |
| **Datum**     | Stores staking information (staker, amount, start slot).                                      |
| **Redeemer**  | Represents the action to be taken (withdrawal).                                               |
| **Validator** | On-chain logic that ensures only the staker can withdraw and receives at least the principal. |
| **Endpoints** | Off-chain functions for staking and unstaking.                                                |

### 3.2 ⚖️ On-Chain vs Off-Chain Logic

* **On-chain logic**: 🧠 Minimal, ensures staker authentication and principal safety.
* **Off-chain logic**: 🧮 Handles reward calculation, transaction building, and wallet interactions.

---

## 4. 🧮 Code Breakdown

### 4.1 📦 Data Types (Datum & Redeemer)

```haskell
data StakeDatum = StakeDatum
    { sdStaker :: PubKeyHash
    , sdAmount :: Integer
    , sdStart  :: Slot
    }

data StakeRedeemer = RedeemWithdraw
```

🧾 **Explanation:**

* `StakeDatum` keeps track of who staked and how much.
* `StakeRedeemer` defines the single allowed action: withdrawal.

### 4.2 🔒 On-Chain Validator Logic

```haskell
mkValidator :: StakeDatum -> StakeRedeemer -> ScriptContext -> Bool
mkValidator dat _ ctx =
    traceIfFalse "not signed by staker" signedByStaker &&
    traceIfFalse "principal not returned" principalReturned
```

🧠 The validator ensures:

* The transaction is signed by the staker ✍️
* The staker receives at least the principal back 💰

### 4.3 🔁 Off-Chain Endpoints

Two endpoints manage interaction:

1. **Stake 💎**: Locks ADA into the contract with datum.
2. **Unstake 💵**: Releases ADA + reward, consuming the script output.

---

## 5. 🚀 Step-by-Step Tutorial

### 5.1 ⚙️ Setting Up the Environment

Ensure you have:

* 🧰 `cabal`, `ghc`, and `plutus-apps` installed.
* 💳 A local testnet wallet for deployment.

### 5.2 🧱 Compiling and Deploying the Contract

1. 💾 Save the contract as `StakingRewards.hs`.
2. 🧩 Use `cabal build` to compile.
3. 🚀 Deploy the validator using the Plutus Application Backend (PAB) or emulator.

### 5.3 💼 Using the Endpoints

**Stake:**

```bash
cabal run stake -- 1000000  # Stake 1 ADA
```

**Unstake:**

```bash
cabal run unstake
```

💡 The reward is calculated off-chain and included in the transaction.

---

## 6. 🧪 Testing and Simulation

### 6.1 🧍‍♂️ How to Simulate Staking

Use the emulator to simulate staking by creating a wallet and calling the `stake` endpoint.

### 6.2 🏁 How to Simulate Reward Withdrawal

Advance the blockchain slot count, then call `unstake` to test reward calculation.

---

## 7. 🛡️ Security and Design Considerations

### 7.1 ⚠️ Potential Risks

* 🚨 **Off-chain reward logic**: Could underpay or skip rewards.
* 🧱 **Single-point failure**: No admin controls or emergency exits.

### 7.2 🧰 Improvements for Production

* 🔄 Move reward calculation on-chain using a **state machine**.
* 💎 Add token staking and time-lock logic.
* 👑 Include admin roles for managing reward rates.

---

## 8. 📖 Glossary

| 🔤 Term       | 🧩 Definition                                                           |
| ------------- | ----------------------------------------------------------------------- |
| **Datum**     | Persistent on-chain data attached to UTXOs.                             |
| **Redeemer**  | Data passed to unlock a UTXO.                                           |
| **Validator** | On-chain logic that validates transactions.                             |
| **Endpoint**  | Off-chain function exposed for interaction.                             |
| **UTXO**      | Unspent Transaction Output — a piece of value stored on the blockchain. |
| **PAB**       | Plutus Application Backend — runs contracts locally for testing.        |
| **Lovelace**  | Smallest unit of ADA (1 ADA = 1,000,000 Lovelace).                      |

---

### 🌈 Final Note

This tutorial provides a foundational understanding of how staking can be implemented in Plutus. You can extend this example by integrating:

* 🧮 On-chain reward logic
* 🎟️ NFTs for staking identity
* 🌍 Integration with DeFi pools
