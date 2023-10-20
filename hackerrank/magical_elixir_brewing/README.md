# Magical Elixir Brewing

This Elixir program is designed to solve the "Magical Elixir Brewing" problem from Hackerrank. The problem can be found [here](https://www.hackerrank.com/contests/the-apprentice-trials-set-1/challenges/magical-elixir-brewing).

## Concepts

### Pipes

Pipes (`|>`) are used for function chaining. It takes the output of the function on its left and passes it as the first argument to the function on its right.

```elixir
nr_wizards * number_servings / @cauldron_servings |> Float.ceil() |> trunc()
```

### Modules, Functions, and Private Functions

In Elixir, a module serves as a namespace for functions. A module is defined using `defmodule` followed by a block containing function definitions (`def`) and private function definitions (`defp`).

#### Public Functions

Public functions are defined using `def` and can be accessed from outside the module. For instance:

```elixir
@doc """
Calculates the minimum number of cauldrons needed.
"""
def min_cauldrons({nr_wizards, number_servings}) do
  # ... code ...
end
```

#### Private Functions

Private functions are defined with `defp` and are only accessible within the module. For example, `read_int` and `read_list` are private functions:

```elixir
defp read_int do
  # ... code ...
end
```

These private functions encapsulate helper behavior that the module needs internally.

📖 Official Documentation: [Modules and Functions](https://elixir-lang.org/getting-started/modules-and-functions.html)

### Tuples and Lists

In this program, tuples and lists are widely used to hold and manipulate data.

#### Tuples

Tuples are ordered collections of elements. A tuple groups the number of wizards with the servings each wizard requires, like so: `{1, 5}`.

```elixir
def min_cauldrons({nr_wizards, number_servings}) do
  # ... code ...
end
```

Here, the function `min_cauldrons` takes a single tuple argument containing two elements: `nr_wizards` and `number_servings`.

#### Lists

Lists are collections that can hold multiple tuples. The function `read_list` returns a list of tuples:

```elixir
defp read_list do
  # ... code ...
end
```

It reads lines from standard input, converts each line to a tuple of integers, and then accumulates these tuples into a list.

📖 Official Documentation: [List](https://hexdocs.pm/elixir/List.html), [Tuple](https://hexdocs.pm/elixir/Tuple.html)

---

Feel free to integrate these updates into your README.md! 😎

### Tuples and Lists

The code uses tuples to group the number of wizards with the servings each requires. Lists of these tuples are manipulated using `Enum.map`.

```elixir
Enum.map(&List.to_tuple(Enum.map(&1, fn x -> String.to_integer(x) end)))
```

### Module Attributes

Module attributes serve as constants in Elixir. In our case, `@cauldron_servings` is set to 4, representing the servings in each cauldron.

```elixir
@cauldron_servings 4
```

### Official Documentation

To read more on these concepts, check out the [official Elixir docs](https://hexdocs.pm/elixir/).

## Testing

The program uses ExUnit for testing, which is built into Elixir. The tests are in the `test/solution_test.exs` file.

To run the tests:

```sh
mix test
```

### DocTests

Module-level documentation (`@doc`) often includes examples of function use. These examples can be executed as tests, known as doctests.

```elixir
@doc """
  iex> Solution.min_cauldrons({10, 5})
  13
"""
```

To run just the doctests:

```sh
mix test --only doctest
```