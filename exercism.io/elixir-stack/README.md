# Porting an Elixir stack example
I took the following Elixir stack example from [this page](https://blog.codeship.com/statefulness-in-elixir/)

```elixir
defmodule Stack do
  def start_link do
    pid = spawn_link(__MODULE__, :loop, [[]])
    {:ok, pid}
  end

  def loop(stack) do
    receive do
      {:size, sender} -> 
        send(sender, {:ok, Enum.count(stack)})
      {:push, item} -> stack = [item | stack]
      {:pop, sender} ->
        [item | stack] = stack
        send(sender, {:ok, item})
    end
    loop(stack)
  end

  def size(pid) do
    send pid, {:size, self}
    receive do {:ok, size} -> size end
  end

  def push(pid, item) do
    send pid, {:push, item}
  end

  def pop(pid) do
    send pid, {:pop, self}
    receive do {:ok, item} -> item end
  end
end
```

and ported it to Racket using a [Places version](https://github.com/lojic/LearningRacket/blob/master/exercism.io/elixir-stack/stack.rkt) and a [Threads version](https://github.com/lojic/LearningRacket/blob/master/exercism.io/elixir-stack/stack-threads.rkt)