# Fund--Governance-Smart-Contract
A multi-signature governance smart contract for Cardano that enables controlled fund management with time-based conditions and committee oversight.
## Table of Contents
## Project Overview

**Fund Governance Smart Contract** is a multi-signature governance system built on the Cardano blockchain that enables controlled fund management with time-based conditions and committee oversight. This project implements a complete governance system that allows an owner to deposit funds, officials to approve releases, and provides mechanisms for both successful fund releases and safety refunds.

### Key Concepts:
- **Multi-signature governance**: Requires multiple officials to approve transactions
- **Time-based conditions**: Enforces deadlines for decision-making
- **Committee oversight**: Designated officials with voting power
- **On-chain validation**: All rules enforced at the blockchain level
- **Secure fund management**: Prevents unauthorized access and ensures proper fund distribution

## Project Requirements

The project successfully implements all four required endpoints:

### ✅ 1. Deposit Endpoint
- **Function**: Initialize the governance contract with funds
- **Rules**: Only the designated owner can deposit funds
- **Validation**: Owner signature verification
- **Output**: Creates initial datum with governance parameters

### ✅ 2. Approve Endpoint
- **Function**: Officials approve fund release
- **Rules**: 
  - Only designated officials can approve
  - Each official can approve only once (unique signatures)
  - Must approve before deadline
- **Validation**: Official signature verification and duplicate check

### ✅ 3. Release Funds
- **Function**: Release funds to owner when conditions met
- **Rules**:
  - Only owner can initiate release
  - Current time must be ≤ deadline
  - Approvals count must be ≥ required approvals
  - Full amount must be sent to owner
- **Validation**: Time, approval count, and amount verification

### ✅ 4. Refund Funds
- **Function**: Refund to owner when conditions not met
- **Rules**:
  - Only owner can initiate refund
  - Current time must be > deadline
  - Approvals count must be < required approvals
  - Full amount must be sent to owner
- **Validation**: Time, insufficient approvals, and amount verification
