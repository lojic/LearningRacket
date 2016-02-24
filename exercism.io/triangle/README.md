# Triangle

Write a program that can tell you if a triangle is equilateral, isosceles, or scalene.

The program should raise an error if the triangle cannot exist.

Tests are provided, delete one `skip` at a time.

## Hint

The sum of the lengths of any two sides of a triangle always exceeds the
length of the third side, a principle known as the _triangle
inequality_.

## Running tests

Execute the tests with:

```bash
$ elixir bob_test.exs
```

(Replace `bob_test.exs` with the name of the test file.)


### Pending tests

In the test suites, all but the first test have been skipped.

Once you get a test passing, you can unskip the next one by
commenting out the relevant `@tag :pending` with a `#` symbol.

For example:

```elixir
# @tag :pending
test "shouting" do
  assert Bob.hey("WATCH OUT!") == "Whoa, chill out!"
end
```

Or, you can enable all the tests by commenting out the
`ExUnit.configure` line in the test suite.

```elixir
# ExUnit.configure exclude: :pending, trace: true
```

For more detailed information about the Elixir track, please
see the [help page](http://exercism.io/languages/elixir).

## Source

The Ruby Koans triangle project, parts 1 & 2 [view source](http://rubykoans.com)
