defmodule Solution do

  defp convert_to_number(string) do
    case Float.parse(string) do
      {number, ""} -> number   # If it's a float, use it as-is.
      :error -> String.to_integer(string)  # If it's not a float, it must be an integer.
    end
  end

  defp read_floats() do
    IO.stream(:stdio, :line)              # Creates a stream from stdin, each item is a line.
      |> Stream.map(&String.trim/1)       # Trims whitespace from each line.
      |> Stream.map(&convert_to_number/1) # Converts each trimmed line to an integer.
      |> Enum.to_list()                   # Converts the stream to a list.
  end

  def factorial(x) when x == 0, do: 1
  def factorial(x) when x > 0, do: x * factorial(x - 1)

  defp add_ten_times(x, times \\ 0, accumulator \\ 0)
  defp add_ten_times(_x, 10, accumulator), do: accumulator
  defp add_ten_times(x, times, accumulator) do
    new_accumalator = accumulator + :math.pow(x, times) / factorial(times)
    add_ten_times(x, times + 1, new_accumalator)
  end

  def solve(x) do
    add_ten_times(x)
  end

  def main() do
    [_nr_input | input] = read_floats()
    input
      |> Enum.map(&solve/1)
      |> Enum.each(&IO.puts/1)
  end

end

Solution.main()
