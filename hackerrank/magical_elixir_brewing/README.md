# Magical Elixir Brewing with Elixir 🍵

Problem is still not really solved, but it soon will be...

https://www.hackerrank.com/contests/the-apprentice-trials-set-1/challenges/magical-elixir-brewing/problem

## The Problem Description 🔮

In this problem, we're given a list of cauldrons, each with a certain amount of liquid, and a target amount `x`. The goal is to find the minimum number of cauldrons needed to meet or exceed `x`. If it's not possible, return -1.

Here's a breakdown of how the Elixir code tackles the problem using different Elixir concepts.

## Atoms ✨

Atoms aren't explicitly used in this problem, but they could be useful for tagging tuples with statuses like `{:ok, value}` or `{:error, reason}`.

## Pattern Matching 🧩

Pattern matching is heavily used to destruct lists and get values from them.

```elixir
[_n, x] = read_list()
cauldrons = read_list()
```

Here, `_n` and `x` are extracted from the list returned by `read_list()`. We do the same for `cauldrons`.

## Case Statements 🎛️

We used `case` to handle different cases when reading input:

```elixir
case IO.gets("") do
  :eof -> :eof
  str -> String.trim(str) |> String.to_integer()
end
```

We check if we have reached the end of the file (`:eof`) or if there's more to read.

## Modules 📦

The entire code is wrapped in a module called `Solution`.

```elixir
defmodule Solution do
  # Functions go here
end
```

## Enumerables 🔄

We use `Enum.each` and `Enum.map` to manipulate lists:

```elixir
Enum.each(1..t, fn _ -> ... end)
Enum.map(&String.to_integer/1)
```

Here, `Enum.each` iterates over each test case, and `Enum.map` is used to convert a list of strings to integers.

## Pipelines 🚀

We use pipelines extensively to chain together transformations:

```elixir
String.trim(str) |> String.split(" ") |> Enum.map(&String.to_integer/1)
```

Here, we trim the string, split it into a list, and then convert each element to an integer.

---

By understanding these concepts, you'll not only be able to understand the provided code better but also write your own Elixir programs more effectively! 😄

Happy coding, and good luck with the Magical Elixir Brewing problem! 🍀