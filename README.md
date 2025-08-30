# AP Statistics PoK Blockchain

A mathematically-precise Haskell implementation of an emergent consensus blockchain system for AP Statistics education. This project embodies a radical pedagogical innovation: students create knowledge through peer attestation rather than consuming pre-determined answers.

## Project Overview

The AP Statistics PoK Blockchain represents a paradigm shift from traditional educational assessment to emergent social consensus. Built on a foundation of 58 irreducible mathematical atoms across 6 subsystems, the system enables students to collectively discover correct answers through peer verification and statistical convergence.

**Core Philosophy: Math-First Development**
- Every component derives from mathematical specification
- 58 atoms provide complete system coverage
- 13 invariants ensure system integrity
- ADR-driven architecture decisions

**Key Innovation: Emergent Consensus**
- No centralized answer keys required
- Student attestations create natural convergence
- Progressive quorum based on consensus quality
- Statistical truth emerges from collective intelligence

**Educational Impact**
- 89 lessons × 3+ attestations = 267+ additional practice opportunities
- Students experience statistics concepts firsthand through consensus mechanics
- Peer learning reinforces understanding through attestation process
- Social accountability creates natural quality pressure

## System Architecture

### Subsystem Overview (58 Total Atoms)

#### Profile Subsystem (11 atoms)
**Purpose:** Identity and cryptographic key management
- **Data Atoms (8):** username, pubkey, privkey, reputationScore, archetype, transactionHistory, seedphrase, wordList
- **Function Atoms (3):** deriveKeysFromSeed, selectRandomWords, calculateArchetype
- **Key Features:** Deterministic key generation, dynamic archetype classification

#### Blockchain Subsystem (25 atoms)
**Purpose:** Consensus mechanism and transaction processing
- **Data Atoms (19):** hash, prevHash, timestamp, nonce, questionId, answerHash, answerText, score, attesterPubkey, signature, txType, isMatch, questionDistribution, mcqDistribution, frqScores, convergence, confidence, anonymousSignature, officialAnswer
- **Function Atoms (6):** sha256Hash, getCurrentTimestamp, validateSignature, calculateConsensus, updateDistributions, detectOutliers
- **Key Features:** Progressive quorum (3-5 attestations), emergent consensus tracking, anti-gaming detection

#### Questions Subsystem (14 atoms)
**Purpose:** Curriculum management and question processing
- **Data Atoms (8):** prompt, rubric, choices, questionType, difficulty, unit, lesson, tags
- **Function Atoms (6):** validateMCQ, scoreFRQ, categorizeByDifficulty, filterByUnit, searchByTags, generateRubric
- **Key Features:** MCQ hash-based voting, FRQ rubric scoring (1-5 scale)

#### Reputation Subsystem (10 atoms)
**Purpose:** Confidence-weighted scoring and incentives
- **Data Atoms (5):** reputationScore, minorityBonus, consensusReward, confidenceWeight, decayFactor
- **Function Atoms (5):** calculateReputation, applyMinorityBonus, updateConsensusReward, weightByConfidence, applyDecay
- **Key Features:** 1.5x minority bonuses, confidence weighting, temporal decay

#### Persistence Subsystem (7 atoms)
**Purpose:** State serialization and data integrity
- **Data Atoms (3):** filePath, checksum, backupPath
- **Function Atoms (4):** saveState, loadState, verifyIntegrity, createBackup
- **Key Features:** Integrity verification, automatic backups

#### UI Subsystem (6 atoms)
**Purpose:** Event handling and view rendering
- **Data Atoms (3):** currentView, eventQueue, renderData
- **Function Atoms (3):** handleEvent, renderView, updateDisplay
- **Key Features:** Event-driven architecture, multiple view modes

### ADR Integration Points

#### ADR-012: Social Consensus
- **Progressive Quorum Implementation:**
  ```
  progressiveQuorum(convergence) = {
    5, if convergence < 0.5    // Low consensus requires more attestations
    4, if 0.5 ≤ convergence < 0.8  // Medium consensus
    3, if convergence ≥ 0.8    // High consensus allows fewer attestations
  }
  ```
- **Peer Attestation:** Eliminates centralized answer keys through collective verification

#### ADR-028: Emergent Attestation with Optional Reveals
- **MCQ Processing:** SHA-256 hash submission prevents gaming while enabling verification
- **FRQ Scoring:** Peer-based rubric scoring with statistical convergence
- **AP Reveals:** Optional anonymous corrections after 50% convergence
- **Anti-Gaming:** 30-day rate limiting, outlier detection, reputation weighting

### Dependency Graph

```
[QUESTIONS] --hashes(MCQ)--> [BLOCKCHAIN] <--attests-- [PROFILE]
    ↓ rubric(FRQ)                ↓                        ↓
    ↓                     emergent_consensus          confidence
    ↓                            ↓                        ↓
[UI] --renders--> distributions  ↓                   [REPUTATION]
    ↑                            ↓                        ↓
events                    progressive_quorum         bonuses(1.5x minority)
    ↑                            ↓                        ↓
[PERSISTENCE] <--saves-- validated_blocks <--rewards------+
                                 ↓
                          AP_reveals(optional)
                                 ↓
                          outlier_detection
```

## Installation and Setup

### Prerequisites
- [Stack](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool)
- Git

### Installation Steps

1. **Clone the repository:**
   ```bash
   git clone <repository-url>
   cd apstat-fundamental-hs
   ```

2. **Build the project:**
   ```bash
   stack build
   ```

3. **Run tests:**
   ```bash
   stack test
   ```

4. **Execute the application:**
   ```bash
   stack exec apstat-fundamental-hs-exe
   ```

### Dependencies
- **cryptonite**: SHA-256 hashing for MCQ answers
- **aeson**: JSON serialization for persistence
- **containers**: Map/Set data structures for distributions
- **time**: Timestamp management for rate limiting
- **mtl**: Monad transformers for state management
- **random**: Cryptographic seed generation

## Usage Guide

### Creating User Profiles
- **Generate Profile:** System creates deterministic seed phrase and derives cryptographic keys
- **Archetype Assignment:** Dynamic classification based on transaction history (Aces, Strategists, Explorers, Learners, Socials)
- **Reputation Initialization:** Starting score with confidence weighting enabled

### Submitting Attestations

#### Multiple Choice Questions (MCQs)
```bash
# Students submit SHA-256 hash of selected option
attest_mcq --question-id "unit1_q5" --answer-hash <sha256_of_choice>
```

#### Free Response Questions (FRQs)
```bash
# Students provide rubric-based scores (1-5 scale)
attest_frq --question-id "unit2_frq1" --score 4.2 --confidence 0.85
```

### Viewing Consensus Status
- **Distribution Tracking:** View real-time answer distributions across peer attestations
- **Convergence Metrics:** 
  - MCQ: `convergence = max_count / total_attestations`
  - FRQ: `convergence = max(0, 1 - (stdDev/mean))`
- **Quorum Status:** Check if sufficient attestations received for consensus

### Reputation Management
- **Score Tracking:** Monitor confidence-weighted reputation accumulation
- **Minority Bonuses:** 1.5x multiplier for correct minority positions
- **Decay Application:** Temporal reduction to encourage continued participation

### UI Navigation
- **ProfileView:** User identity and reputation management
- **BlockchainView:** Transaction history and consensus status
- **QuestionsView:** Available questions and attestation opportunities
- **ReputationView:** Detailed scoring and bonus tracking
- **MainMenu:** System navigation and settings

## Invariants and Verification

The system enforces 13 mathematical invariants to ensure integrity and correctness:

### Core Identity and Security
1. **Identity Invariant:** `∀ transaction t: t.attesterPubkey ∈ {p.pubkey | p ∈ profiles}`
   - Every transaction must be signed by a valid user profile

2. **Hash Validation:** `∀ MCQ answer a: sha256Hash(a.choice) = a.answerHash`
   - MCQ submissions must provide valid SHA-256 hash proofs

3. **Temporal Ordering:** `∀ timestamp t: t.current > t.previous`
   - Transactions must maintain chronological sequence

### Consensus Mechanisms
4. **Progressive Quorum:** `∀ block b: |b.attestations| ≥ progressiveQuorum(b.convergence)`
   - Block finalization requires convergence-based attestation thresholds

5. **Convergence Calculation:**
   - MCQ: `convergence = max_count / total_attestations`
   - FRQ: `convergence = max(0, 1 - (stdDev/mean))`

6. **Rate Limiting:** `∀ user u, question q: timeSinceLastAttestation(u, q) > 30 days`
   - Prevents gaming through repeated attestation attempts

### Scoring and Reputation
7. **FRQ Scoring Bounds:** `∀ FRQ response r: 1.0 ≤ scoreFRQ(r, rubric) ≤ 5.0`
   - Free response scores must remain within rubric bounds

8. **Confidence-Weighted Rewards:** 
   ```
   p.reputationScore = Σ(applyBonuses(consensus(a), confidence(a)) × minorityBonus(a) × decayScores(time))
   ```
   - Reputation incorporates confidence levels and minority position bonuses

9. **Outlier Detection:** `∀ attestation a: detectOutliers([a]) = [] ∨ flagForReview(a)`
   - Statistical analysis identifies and flags anomalous voting patterns

### System Integrity
10. **Cycle Stability:** System dependency graph remains DAG except reputation feedback loop
    - Prevents circular dependencies that could destabilize consensus

11. **Persistence Integrity:** `∀ state s: loadState(saveState(s)) = s`
    - Data serialization maintains perfect fidelity

12. **Atomicity:** `∀ atom a ∈ S: independent(a) ⇒ testable(a)`
    - Each system component remains independently verifiable

13. **UI Safety:** `∀ state s: renderState(s) ≠ null ∧ validView(s.currentView)`
    - User interface maintains valid state across all interactions

## Testing

### Test Coverage
- **Total Tests:** 205 comprehensive test cases
- **Current Status:** 200 passing, 5 legacy failures
- **Coverage Distribution:**
  - ProfileSpec: Identity management and key derivation
  - BlockchainSpec: Consensus mechanisms and transaction processing
  - QuestionsSpec: MCQ/FRQ handling and rubric validation
  - ReputationSpec: Scoring algorithms and bonus calculations
  - PersistenceSpec: State serialization and integrity verification
  - UISpec: Event handling and view rendering

### Running Tests
```bash
# Execute all tests
stack test

# Run specific subsystem tests
stack test --test-arguments "--match Profile"
stack test --test-arguments "--match Blockchain"
```

### Test Philosophy
- **Atom-Level Testing:** Each of the 58 atoms has dedicated test coverage
- **Invariant Verification:** All 13 system invariants tested under various conditions
- **Property-Based Testing:** QuickCheck integration for mathematical property verification
- **Integration Testing:** Cross-subsystem interaction validation

## Limitations and Extensions

### Current Limitations
- **Legacy Test Failures:** 5 tests require resolution for complete system validation
- **Network Layer:** Current implementation uses local persistence only; P2P networking planned
- **UI Implementation:** Console-based interface; web UI under development
- **Scalability:** Current design optimized for classroom sizes (20-30 students)

### Extension Guidelines

#### Adding New Atoms
1. **Maintain Independence:** New atoms must not create circular dependencies
2. **Follow Subsystem Patterns:** Integrate within existing architectural boundaries
3. **Preserve Invariants:** Ensure all 13 system invariants remain satisfied
4. **Comprehensive Testing:** Add full test coverage including property-based tests

#### Architectural Constraints
- Reference `docs/FUNDAMENTAL.md` for all structural decisions
- New features must trace back to mathematical specifications
- ADR process required for significant changes
- Maintain 58-atom total through careful decomposition

#### Recommended Extensions
- **Network Layer:** P2P gossip protocol for distributed consensus
- **Advanced Analytics:** Student learning pattern analysis
- **Gamification:** Achievement system based on consensus participation
- **Teacher Dashboard:** Real-time classroom consensus monitoring
- **Mobile Interface:** Native mobile app for increased accessibility

### Contributing
- Follow math-first development philosophy
- Ensure all code changes reference specific atoms or invariants
- Maintain comprehensive test coverage
- Use ADR process for architectural decisions

---

This project demonstrates how mathematical rigor combined with emergent social consensus can revolutionize educational assessment. By eliminating authoritative answer keys and empowering students to collectively discover knowledge, we create a more engaging, statistically-grounded learning experience that prepares students for real-world collaborative problem-solving.