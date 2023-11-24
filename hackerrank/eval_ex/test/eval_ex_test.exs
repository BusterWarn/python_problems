defmodule SolutionTest do
  use ExUnit.Case
  doctest Solution

  test "Solve Input" do
    assert Solution.solve(0) === 0
  end
end
