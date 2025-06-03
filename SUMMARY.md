# Repository Summary: Analytical Engine

This repository contains a Haskell implementation of Charles Babbage's Analytical Engine.

## Functionality

The program simulates the operations of the Analytical Engine by:

1.  **Reading Input:** It takes "programs" as input, which are defined by a set of "PunchCard" files.
    *   `operations.pc`: Specifies the sequence of arithmetic and control flow operations (e.g., Add, Subtract, Load, Store, Move Forwards/Backwards on the operation chain).
    *   `numbers.pc`: Provides the initial numerical values for the engine's "Store" (its memory).
    *   `loadStore.pc`: Defines which memory locations in the Store are to be used by the operations.

2.  **PunchCard Format:** The `.pc` files adhere to a specific textual format representing punched holes on cards, as detailed in the `README.md`. Different patterns of "holes" (represented by characters like `*`) define specific operations, digits of numbers, or memory addresses.

3.  **Core Components & Execution:**
    *   **Mill:** The calculating unit of the engine. In the code (`app/Main.hs`), this is implicitly represented by how operations modify numerical values.
    *   **Store:** The memory of the engine, holding numerical values. It's represented as a list of integers (`StoreValue`) in the Haskell code.
    *   **Operations:** The program defines data types for different kinds of operations:
        *   `MathsOperation`: Basic arithmetic (Add, Subtract, Multiply, Divide).
        *   `VariableOperation`: Operations that interact with the Store (Supply/Load, Store) or control program flow (Forwards, Backwards).
    *   **Minecart (`app/Minecart.hs`):** This module implements a concept analogous to the mechanism that would feed operation cards into the engine. It's a data structure (like a zipper or focused list) that holds the sequence of operations and allows stepping through them (`next`, `prev`).
    *   **Engine Simulation (`app/Main.hs`):**
        *   The `applyParameters` function is central to the simulation. It takes the initial numbers (from `numbers.pc`) and a sequence of operations (derived from `operations.pc` and `loadStore.pc`).
        *   It processes each operation, updating the state of the Mill (intermediate calculation values) and the Store (memory values).
        *   The `bindOperations` function links the abstract operations from `operations.pc` with the specific memory addresses from `loadStore.pc`.
    *   **Output:** The program can output the state of the Store after processing the operations, effectively showing the results of the computation.

In essence, this project is a software simulation that aims to model the logical structure and operational flow of Babbage's proposed mechanical computer, executing programs defined in a format inspired by its historical design.
