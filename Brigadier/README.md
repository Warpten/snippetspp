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
        Brigadier::Command("foo", [](uint32_t, Brigadier::QuotedString) noexcept { }),
        Brigadier::Node("bar")
            .Then(
                Brigadier::Command("biz", [](S&, uint32_t) noexcept { }),
            )
    );

int main() {
    S source { };
    Brigadier::StringReader reader(command0);
    // parse will be nullptr if parsing failed.
    auto parse = Brigadier::TreeParser<S>::Parse(reader, tree);
    parse(source);
}
```

The above code registers 2 commands:

```
> foo foo $x
> foo bar biz $x
```

## Features

- ❌ Optional parameters (via `std::optional<T>`)
- ❌ Command help
- ❌ Error reporting

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
    template <typename Fn>
    static auto _Extract(CommandNode<Fn> const&, S&, StringReader& reader) noexcept;
    
    //> Returns true if the parameter could be extracted from input.
    static auto _TryExtract(StringReader&) noexcept;
};
```

### Why 'Brigadier' ?

This was ||inspired by|| ~~stolen from~~ Mojang's [eponymous library](https://github.com/Mojang/brigadier). 

### I want benchmarks

Here are some completely irrelevant benchmarks, done with [Nanobench](https://github.com/martinus/nanobench), with numbers you should not care about.
```
|               ns/op |                op/s |    err% |     total | benchmark
|--------------------:|--------------------:|--------:|----------:|:----------
|              104.81 |        9,540,767.85 |    3.1% |      1.25 | `foo foo 42 "bar\"itone" (Parsing)`
|              309.57 |        3,230,277.78 |    2.2% |      3.74 | `foo foo 42 "bar\"itone" (Execute)`
|              108.66 |        9,202,952.16 |    1.1% |      1.32 | `foo bar biz 42 (Parsing)`
|              120.65 |        8,288,628.37 |    1.5% |      1.45 | `foo bar biz 42 (Execute)`
|              131.12 |        7,626,504.58 |    1.6% |      1.60 | `foo bar biz "foo" (Parsing)`
|              191.85 |        5,212,362.05 |    2.0% |      2.31 | `foo bar biz "foo" (Execute)`
```

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
        b.run(fmt::format("{} (Parsing)", command), [&]() {
            Brigadier::StringReader reader(command);
            ankerl::nanobench::doNotOptimizeAway(
                (Brigadier::TreeParser<S>::Parse(reader, tree), 0)
            );
        }); 
    };
    auto declareExecuteBench = [&b, &source](const char* command) {
        b.run(fmt::format("{} (Execute)", command), [&]() {
            Brigadier::StringReader reader(command);
            ankerl::nanobench::doNotOptimizeAway(
                (Brigadier::TreeParser<S>::Parse(reader, tree)(source), 0)
            );
        }); 
    };

    declareParseBench(command0);
    declareExecuteBench(command0);

    declareParseBench(command1);
    declareExecuteBench(command1);

    declareParseBench(command2);
    declareExecuteBench(command2);
    
    return EXIT_SUCCESS;
}
```
