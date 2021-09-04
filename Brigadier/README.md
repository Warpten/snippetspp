# Brigadier

A simple command parser.

## Requirements

* C++17 or newer
* Boost (Boost.CallableTraits, Boost.Range)
* fmt

## Usage

```cpp
constexpr static const Brigadier::Tree tree = Brigadier::Node("foo")
    .Then(
        Brigadier::Command("foo", [](uint32_t, Brigadier::QuotedString const&) noexcept { }),
        Brigadier::Node("bar")
            .Then(
                Brigadier::Command("biz", [](S&, uint32_t) noexcept { }),
            )
    );

int main() {
    S source { };
    Brigadier::StringReader reader(command0);
    // parse will be true if parsing was successful.
    auto parse = Brigadier::TreeParser<S>::Parse(reader, tree, source);
}
```

The above code registers 2 commands:

```
> foo foo $x
> foo bar biz $x
```

## Features

- ✔️ Optional parameters (via `std::optional<T>`)
- ❌ Error reporting (In progress)
- ❌ Command help

## API

### `Brigadier::Node(std::string_view)`

This creates an object representing a node of the command tree, which typically will hold subnodes.

### `Brigadier::CommandNode<Fn>(std::string_view, Fn&&)`

This creates an object representing an execution point of the command tree.
- `Fn`
  A [Callable](https://en.cppreference.com/w/cpp/named_req/Callable) to be called when the command is selected during parsing.  
  Signature must be akin to `void(Args...) noexcept`.  
  Arguments supported out of the box are `(u)int(64|32|16)_t`, `float`, `double`, `std::string`.  
  Brigadier also exposes `Word`, `GreedyString` and `QuotedString`, which are explicitely convertible to `std::string`.  
  You can also inject `Source const&` or `Source &` at any point in the parameters. Usually, for simplicity, keep it in front.

### `_ParameterExtractor<S, T, Enable = void>`

This object is the main extendability point of Brigadier. It allows you to define your own parameter types and how they should be parsed. The contract for this object looks as follows:

```cpp
template <typename S, typename T>
struct _ParameterExtractor<S, T> {
    //> Returns effectively T. For references, wrap in std::ref/std::cref.
    //> If this can fail, prefer returning Errorable<T>::MakeSuccess / Errorable<T>::MakeError
    template <typename Fn>
    static auto _Extract(CommandNode<Fn> const&, S&, std::string_view& reader) noexcept;
};
```

### Why 'Brigadier' ?

This was ~~stolen from~~ inspired by Mojang's [eponymous library](https://github.com/Mojang/brigadier). 

### I want benchmarks

Here are some completely irrelevant benchmarks, done with [Nanobench](https://github.com/martinus/nanobench), with numbers you should not care about.

|               ns/op |                op/s |    err% |     total | benchmark
|--------------------:|--------------------:|--------:|----------:|:----------
|              266.82 |        3,747,896.97 |    0.7% |      3.19 | `foo foo 42 "bar\"itone"`
|              234.23 |        4,269,380.00 |    1.5% |      2.85 | `foo bar biz 42`
|              255.48 |        3,914,185.25 |    0.3% |      3.05 | `foo bar biz "foo"`

```cpp
constexpr static const Brigadier::Tree tree = Brigadier::Node("foo")
    .Then(
        Brigadier::Command("foo", [](uint32_t, Brigadier::QuotedString) noexcept { }),
        Brigadier::Node("bar")
            .Then(
                // This is doable but not recommended, since ordering matters.
                Brigadier::Command("biz", [](uint32_t) noexcept { }),
                Brigadier::Command("biz", [](S&, Brigadier::QuotedString) noexcept { })
            )
    );

constexpr static const char command0[] = "foo foo 42 \"bar\\\"itone\"";
constexpr static const char command1[] = "foo bar biz 42";
constexpr static const char command2[] = "foo bar biz \"foo\"";

int main() {
    S source { };
    auto b = ankerl::nanobench::Bench().minEpochIterations(1000000);
    
    auto declareParseBench = [&b, &source](const char* command) {
        b.run(command, [&]() {
            ankerl::nanobench::doNotOptimizeAway(
                Brigadier::TreeParser<S>::Parse(command, tree, source)
            );
        }); 
    };

    declareParseBench(command0);
    declareParseBench(command1);
    declareParseBench(command2);
    
    return EXIT_SUCCESS;
}
```
