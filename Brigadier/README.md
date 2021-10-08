# Brigadier

A simple command parser.

## Requirements

* C++17 or newer
* Boost (Boost.CallableTraits)
* fmt (or `<format>`)

## Usage

### Parse an input

```cpp
constexpr static const Brigadier::Tree tree = Brigadier::Node("foo")
    .Then(
        Brigadier::Command(
            "foo",
            "This command does X thing",
            [](S const&, uint32_t, std::optional<Brigadier::QuotedString>) noexcept { },
            Brigadier::ParameterMeta<uint32_t> { "nodeIndex" },
            Brigadier::ParameterMeta<std::optional<Brigadier::QuotedString>> { "path" }
        ),
        Brigadier::Node("bar")
            .Then(
                Brigadier::Command("biz", [](uint32_t) noexcept { }),
                Brigadier::Command("baz", [](uint32_t) noexcept { }),
                Brigadier::Command("buz", [](uint32_t) noexcept { }),
            )
    );

int main() {
    S source { };
    Brigadier::StringReader reader(command0);
    // parse will be true if parsing was successful.
    auto parse = Brigadier::TreeParser<S>::Parse(reader, tree, source);
}
```

The above code registers 4 commands:

```
> foo foo $x
> foo bar biz $x
> foo bar baz $x
> foo bar buz $x
```

### Print command help

`Brigadier::TreeParser<S>::PrintHelp` expects a generic `Printer` type, which is expected to have the following contract:

```cpp
struct
{
    void NotifyBeginCommand() { } // ⚠️ Required
    void NotifyEndCommand() { } // ⚠️ Required

    void NotifyLiteral(std::string_view literal) {  } // ⚠️ Required
    void NotifyParameter(std::string_view name, bool required) { } // ⚠️ Required
    void NotifyCommandDescription(std::string_view description) { } // ⚠️ Required
    
    
    template <typename T>
    void NotifyParameterDescription(std::string_view name, std::optional<std::string_view> description, bool required) { }
    // -- or -- 
    void NotifyParameterDescription(std::string_view name, std::optional<std::string_view> description, bool required) { }
    // The former will pass parameter type through T.
};
```

Pass an instance of a type matching this interface to the `PrintHelp` method. A basic output to console could look like this when executed against the command tree shown above:
```
> PrintHelp("foo foo", tree, printer);

foo foo [nodeIndex] <path> 
  This command does X thing
  - nodeIndex (Required) unsigned int
  - path (Optional) std::optional<Brigadier::QuotedString>

> PrintHelp("foo bar", tree, printer);

foo bar biz [parameters...] 
foo bar boz [parameters...] 
foo bar buz [parameters...] 
```
Example of a more involved usage:

![oYafEu9EDC](https://user-images.githubusercontent.com/563936/134428322-ff47065e-4a48-4999-9d0e-6ae3b75fee61.gif)

## Features

- ✔️ Optional parameters (via `std::optional<T>`)
- ✔️ Command help
- ❌ `std::variant<Ts...>` parameters

## API

### `Brigadier::Node(std::string_view)`

This creates an object representing a node of the command tree, which typically will hold subnodes.

### `Brigadier::Command<Fn>(std::string_view literal, Fn&& fn)`
### `Brigadier::Command<Fn>(std::string_view literal, std::string_view description, Fn&& fn, parameterInfo...)`

This creates an object representing an execution point of the command tree.
- `literal` The command literal.
- `description` A short description of the command.
- `fn`
  A [Callable](https://en.cppreference.com/w/cpp/named_req/Callable) to be called when the command is selected during parsing.  
  Signature must be akin to `void(Args...) noexcept`.  
  Arguments supported out of the box are `(u)int(64|32|16)_t`, `float`, `double`, `std::string`.  
  Brigadier also exposes `Word`, `GreedyString` and `QuotedString`, which are explicitely convertible to `std::string`.  
  You can also inject `Source const&` or `Source &` at any point in the parameters. Usually, for simplicity, keep it in front.
- `parameterInfo`  
  One or more of `Brigadier::ParameterMeta<T>`. Must match the order and types of parameters of `fn` that are extracted from the command.  
  ⚠️ Types are there to ensure any change to the callback is mirrored on the parameter metadata, and vice-versa.

### `_ParameterExtractor<S, T, Enable = void>`

This object is the main extendability point of Brigadier. It allows you to define your own parameter types and how they should be parsed. The contract for this object looks as follows:

```cpp
template <typename T>
struct _ParameterExtractor<T> {
    //> Returns effectively T. For references, wrap in std::ref/std::cref.
    //> *Always* return an std::optional.
    template <typename Fn>
    static auto _Extract(std::string_view& reader) noexcept;
};
```

### Why 'Brigadier' ?

This was ~~stolen from~~ inspired by Mojang's [eponymous library](https://github.com/Mojang/brigadier). 

### I want benchmarks

Here are some completely irrelevant benchmarks, done with [Nanobench](https://github.com/martinus/nanobench), with numbers you should not care about.

|               ns/op |                op/s |    err% |     total | benchmark
|--------------------:|--------------------:|--------:|----------:|:----------
|              228.45 |        4,377,258.94 |    3.0% |      2.78 | `foo foo 42 "bar\"itone"`
|              100.23 |        9,977,433.52 |    2.0% |      1.19 | `foo bar biz 42`
|              160.88 |        6,215,815.14 |    1.8% |      1.93 | `foo bar biz "foo"`
|               60.12 |       16,633,191.67 |    3.8% |      0.71 | `PrintHelp("foo foo", tree, printer)`

The help print test is mostly a no-op but shows the cost of traversing a simple tree.

```cpp

struct HelpPrinter
{
    void NotifyBeginCommand() { } // ⚠️ Required
    void NotifyEndCommand() { } // ⚠️ Required

    void NotifyLiteral(std::string_view literal) {  } // ⚠️ Required
    void NotifyParameter(std::string_view name, bool required) { } // ⚠️ Required
    void NotifyCommandDescription(std::string_view description) { } // ⚠️ Required
    
    
    template <typename T>
    void NotifyParameterDescription(std::string_view name, std::optional<std::string_view> description, bool required) { }
    // -- or -- 
    void NotifyParameterDescription(std::string_view name, std::optional<std::string_view> description, bool required) { }
    // The former will pass parameter type through T.
};

constexpr static const Brigadier::Tree tree = Brigadier::Node("foo")
    .Then(
        Brigadier::Command(
            "foo",
            "This command does X thing",
            [](S const&, uint32_t, std::optional<Brigadier::QuotedString>) noexcept { },
            Brigadier::ParameterMeta<uint32_t> { "nodeIndex" },
            Brigadier::ParameterMeta<std::optional<Brigadier::QuotedString>> { "path" }
        ),
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
    
    HelpPrinter printer;
    b.run("PrintHelp(\"foo foo\", tree, printer)", [&]() {
        ankerl::nanobench::doNotOptimizeAway(
            (Brigadier::TreeParser<S>::PrintHelp("foo foo", tree, printer), 0)
        );
    });
    
    return EXIT_SUCCESS;
}
```
