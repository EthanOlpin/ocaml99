Solutions to [99 Problems in OCaml](https://v2.ocaml.org/learn/tutorials/99problems.html)

## Running the solutions

1. Clone the repo
2. Switch to the new directory
3. Create an empty switch

    ```
    opam switch create ocaml99 5.1.0
    ```
4. Install the project dependencies

    ```
    opam install . --locked
    ```
5. Run the included unit tests

    ```
    dune test
    ```
