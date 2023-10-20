defmodule Solution do


  @doc """
  Reads a line from standard input, trims any whitespace, and converts it to an integer.

  ## Return Value
  Returns an integer from the read line.

  ## Note
  This function is private and should only be used within this module.
  """
  defp read_int do
    IO.gets("")
      |> String.trim
      |> String.to_integer
  end

  @doc """
  Reads a series of lines from standard input and converts them into a list of tuples.

  Each line should contain two space-separated integers. The function will read each line,
  trim any extra whitespace, and then convert the numbers into integers. These integers are
  then made into tuples.

  ## Example Input from Standard IO
      "1 5\n2 6\n4 3\n3 5\n"

  ## Return Value
  Returns a list of tuples where each tuple contains two integers from each line:
  [ {1, 5}, {2, 6}, {4, 3}, {3, 5} ]

  ## Note

  This function is private and should only be used within this module.
  """
  defp read_list do
    IO.stream(:stdio, :line)
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.split(&1, " "))
      |> Enum.map(&List.to_tuple(Enum.map(&1, fn x -> String.to_integer(x) end)))
  end

  @cauldron_servings 4
  @doc """
  Calculates the minimum number of cauldrons needed to satisfy all the wizards.

  ## Parameters
  - `{nr_wizards, number_servings}`: A tuple where `nr_wizards` is the number of wizards and `number_servings`
                                     is the number of servings each wizard requires.

  ## Examples
      iex> Solution.min_cauldrons({10, 5})
      13
      iex> Solution.min_cauldrons({10, 6})
      15
  """
  def min_cauldrons({nr_wizards, number_servings}) do
    nr_wizards * number_servings / @cauldron_servings |> Float.ceil() |> trunc()
  end

  @doc """
  Entry point for the program.
  """
  def main do

    # We don't care about the first input
    _ = read_int()
    lines = read_list()

    # Map through lines and apply function to it.
    Enum.each(lines, fn line ->
      IO.puts(min_cauldrons(line))
    end)

  end
end

# Uncomment when submitting to hackerrank
Solution.main()
