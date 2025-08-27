# APStat Fundamental

## Philosophy: Math-First Development

This project follows a strict "math-first" approach where all structure and code must derive from the mathematical specification.

## Artifact Hierarchy

### 1. The Constitution (Absolute Priority)
- **File**: `docs/FUNDAMENTAL.md`
- **Role**: Immutable blueprint containing the 58-atom model, 13 invariants, and ADRs
- **Priority**: Single source of truth - all development must align with this document

### 2. The Logical Reference
- **File**: `reference/racket-digital-twin.rkt`
- **Role**: Purest existing code embodiment of the mathematical logic
- **Priority**: Reference for functional implementation patterns

### 3. Validation Data (Secondary)
- **Files**: `data/curriculum.json`, `data/allUnitsData.js`
- **Role**: Final testing and validation only
- **Priority**: Not core to initial structure, may be refactored

### 4. Source Code (Future)
- **Directory**: `src/`
- **Role**: To be populated with math-derived implementations
- **Priority**: Must derive from mathematical specifications in FUNDAMENTAL.md

## Development Principle

Every code change must trace back to the mathematical foundation. Implementation decisions should reference specific atoms, invariants, or ADRs from the constitution.
