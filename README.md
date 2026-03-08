## Regular Expressions in Haskell

This project is a lightweight regular expression engine built in Haskell that evaluates strings by constructing and traversing a **Non-deterministic Finite Automaton (NFA)**.

---

### Usage
The program evaluates a regular expression `r` against a string `s` to determine if `s` is accepted by the language defined by `r`.

### Syntax Rules
The engine supports the following operations for constructing regular expressions:

| Operation | Syntax | Description |
| :--- | :--- | :--- |
| **Concatenation** | `r1r2` | Matches `r1` immediately followed by `r2`. |
| **Option Group** | `[abcd]` | Matches a **single** character from the set (e.g., 'a', 'b', 'c', or 'd'). |
| **Kleene Star** | `(r)` | Represents the Kleene closure; matches `r` zero or more times. |

---

### Technical Constraints
* **Alphabet:** Currently supports `a-z`.
* **Reserved Characters:** The characters `@`, `[`, `]`, `(`, and `)` are reserved for internal logic and operators. 
* **Extensibility:** The character set can be updated by modifying the alphabet definition within the source code.
