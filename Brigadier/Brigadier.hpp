/* 
 * This file is part of the snippetspp distribution (https://github.com/Warpten/snippetspp or http://xxx.github.io).
 * Copyright (c) 2021 Warpten.
 * 
 * This program is free software: you can redistribute it and/or modify  
 * it under the terms of the GNU General Public License as published by  
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License 
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include <charconv>
#include <functional>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <vector> 

#include <boost/callable_traits/args.hpp>
#include <boost/callable_traits/is_noexcept.hpp>
#include <boost/range/adaptor/filtered.hpp>
#include <boost/range/size.hpp>

#include <fmt/format.h>

namespace Brigadier {
    namespace Details {
        template <typename... Ts> void Sink(Ts&&...) { }
    }

    // Forward declarations vvv

    template <typename>
    constexpr static const bool dependant_false = false;

    struct String;
    template <typename> struct Errorable;
    struct StringReader;

    struct Word;
    struct GreedyString;
    struct QuotedString;

    template <typename, typename> struct ParseResult;
    template <typename> struct ArgumentType;
    
    template <typename, typename> struct LiteralTreeNode;
    template <typename, typename> struct CommandDispatcher;

    template <typename, typename> struct _ParameterExtractor;

}

namespace Brigadier {
    // Declarations vvv
    struct String
    {
        String(const std::string& value);
        virtual ~String() = default;

        operator std::string() const;

    private:
        std::string _value;
    };

    template <typename T>
    struct Errorable final
    {
        bool Success;
        union {
            T Value;
            std::string Error;
        };

    private:
        Errorable(T&& value);
        Errorable(T const& value);
        Errorable(std::string const& error, void*);

        Errorable(Errorable<T> const& other) : Success(other.Success)
        {
            if (Success)
                Value = other.Value;
            else
                Error = other.Error;
        }

    public:
        ~Errorable() { }

    public:
        static Errorable<T> MakeError(std::string const& reason);

        static Errorable<T> MakeSuccess(T value);
        
        template <typename U>
        Errorable<U> Convert(std::function<U(T const&)> converter);
    };

    struct StringReader final
    {
        static constexpr const char Escape = '\\';
        static constexpr const char SingleQuote = '\'';
        static constexpr const char DoubleQuote = '"';

        explicit StringReader(std::string_view str, size_t cursor = 0);

        StringReader(StringReader&& other) noexcept;

        StringReader(StringReader const& other);

        Errorable<std::string> ReadString();

        Errorable<std::string> ReadQuotedString();

        Errorable<std::string> ReadUnquotedString();

        template <typename T, 
            typename... Args, 
            typename = std::enable_if_t<std::is_standard_layout_v<T> && std::is_trivial_v<T>>
        >
        auto Read(Args&&... args) -> Errorable<typename ArgumentType<T>::type>;

        void Seek(int32_t delta);
        void Skip(size_t count = 1);
        bool CanRead() const;
        char Peek() const;

        size_t GetCursor() const;
        void SetCursor(size_t cursor);

        bool StartsWith(std::string_view str) const;

        std::string_view GetRemaining() const;

    private:
        Errorable<std::string> ReadStringUntil(char terminator);

    private:
        std::string_view _str;
        size_t _cursor;
    };

    struct Word         : String { using String::String; };
    struct GreedyString : String { using String::String; };
    struct QuotedString : String { using String::String; };

    template <typename T> struct ArgumentType {
        using type = T;
        constexpr static const bool optional = false;

        static Errorable<T> Read(StringReader& reader);
    };

    template <typename T> struct ArgumentType<std::optional<T>>
    {
        using type = std::optional<T>;
        constexpr static const bool optional = true;

        static Errorable<std::optional<T>> Read(StringReader& reader);
    };

    template <typename S, typename R>
    struct LiteralTreeNode
    {
        LiteralTreeNode(std::string_view literal);

        LiteralTreeNode(LiteralTreeNode<S, R> const&) = delete;

        LiteralTreeNode(LiteralTreeNode&& other) noexcept;

        ParseResult<S, R> Parse(StringReader& reader) const noexcept;

        LiteralTreeNode<S, R>& Then(std::string const& literal);

        template <typename F>
        LiteralTreeNode<S, R>& Then(std::string const& literal, F&& fn);

        R Execute(S const& source, StringReader& input) const;

        template <typename F>
        LiteralTreeNode<S, R>& Executes(F fn);

        template <typename F, typename... Ts>
        static R _ApplyCall(F fn, S const& source, StringReader& reader, std::tuple<Ts...>* tpl);

        template <typename... Ts>
        auto _ValidateParameters(StringReader& reader, std::tuple<Ts...>* tpl);

        // Extracted as a static method because a lambda would cause
        // zero-initialization of an auto parameter for no reason
        // (fixed in C++20 with god-awful syntax)
        template <typename T>
        static std::optional<T> _SafeExtractParameter(S const& source, StringReader& reader, size_t& itr, size_t count);

        template <typename T, typename... Ts>
        static std::optional<std::string> _ValidateParameters(StringReader& reader, size_t itr, size_t count);

    private:
        std::string _literal;

        std::function<ParseResult<S, R>(StringReader&)> _condition;
        std::function<R(S const&, StringReader&)> _handler;

        std::vector<LiteralTreeNode<S, R>> _children;
    };
    
    template <typename S, typename R>
    struct ParseResult
    {
        ParseResult(LiteralTreeNode<S, R> const& node, StringReader const& input);

        template <typename... Ts>
        ParseResult(std::string const& f, Ts&&... args);

        R Execute(S const& source);

        operator bool() const;

    private:
        std::optional<std::reference_wrapper<const LiteralTreeNode<S, R>>> _node;
        std::optional<StringReader> _input;
        std::optional<std::string> _errorString;
    };


    template <typename S, typename R>
    struct CommandDispatcher
    {
        R Parse(S const& source, std::string_view string);
        R Parse(S const& source, StringReader& reader);

        LiteralTreeNode<S, R>& Then(std::string_view literal);

        template <typename F>
        LiteralTreeNode<S, R>& Then(std::string_view literal, F&& fn);

    private:
        std::unordered_map<std::string, LiteralTreeNode<S, R>> _children;
    };

    // Helpers declarations and implementations
    
    template <typename S, typename T>
    struct _ParameterExtractor {

        using type = decltype(ArgumentType<std::decay_t<T>>::Read(std::declval<StringReader&>()).Value);

        static auto Extract(StringReader& reader, S const& /* source */) {
            reader.Skip();
            auto result = ArgumentType<std::decay_t<T>>::Read(reader);
            return result.Value;
        }
    };

    template <typename S>
    struct _ParameterExtractor<S, S const&> {
        using type = S const&;

        static S const& Extract(StringReader& /* reader */, S const& source) { return source; }
    };

}

namespace Brigadier
{
    String::String(const std::string& value): _value(value) { }

    String::operator std::string() const {
        return _value;
    }

    // ^^^ String / Errorable<T> vvv

    template <typename T>
    Errorable<T>::Errorable(T&& value): Success(true), Value(std::move(value)) { }

    template <typename T>
    Errorable<T>::Errorable(T const& value): Success(true), Value(value) { }

    template <typename T>
    Errorable<T>::Errorable(std::string const& error, void*): Success(false), Error(error) { }

    template <typename T>
    Errorable<T> Errorable<T>::MakeError(std::string const& reason) { return { reason, nullptr }; }

    template <typename T>
    Errorable<T> Errorable<T>::MakeSuccess(T value) { return { std::move(value) }; }

    template <typename T>
    template <typename U>
    Errorable<U> Errorable<T>::Convert(std::function<U(T const&)> converter) {
        if (Success)
            return Errorable<U>::MakeSuccess(converter(Value));

        return Errorable<U>::MakeError(Error);
    }

    // ^^^ Errorable<T> / StringReader vvv

    StringReader::StringReader(std::string_view str, size_t cursor): _str(str), _cursor(cursor) { }

    StringReader::StringReader(StringReader&& other) noexcept
        : _str(std::move(other._str)), _cursor(std::move(other._cursor)) {
        other._cursor = std::numeric_limits<size_t>::max();
    }

    StringReader::StringReader(StringReader const& other): _str(other._str), _cursor(other._cursor) { }

    Errorable<std::string> StringReader::ReadString()
    {
        if (!CanRead())
            return Errorable<std::string>::MakeError("Cannot read");

        char next = Peek();
        if (next == SingleQuote || next == DoubleQuote)
        {
            Skip();
            return ReadStringUntil(next);
        }

        return ReadUnquotedString();
    }

    Errorable<std::string> StringReader::ReadQuotedString()
    {
        if (!CanRead())
            return Errorable<std::string>::MakeError("Cannot read");

        char next = Peek();
        if (next != SingleQuote && next != DoubleQuote)
            return Errorable<std::string>::MakeError("Expected an opening quote");

        Skip();
        return ReadStringUntil(next);
    }

    Errorable<std::string> StringReader::ReadUnquotedString()
    {
        uint32_t startCursor = _cursor;

        static auto isAllowedCharacter = [](char c)
        {
            return c >= '0' && c <= '9'
                || c >= 'A' && c <= 'Z'
                || c >= 'a' && c <= 'z'
                || c == '_' || c == '-'
                || c == '.' || c == '+';
        };

        while (CanRead() && isAllowedCharacter(Peek()))
            Skip();

        return Errorable<std::string>::MakeSuccess(std::string {
            _str.substr(startCursor, _cursor - startCursor + 1)
        });
    }

    void StringReader::Seek(int32_t delta) {
        _cursor = std::max<int32_t>(_cursor + delta, 0);
    }

    void StringReader::Skip(size_t count) {
        _cursor += std::max<size_t>(count, 1u);
    }

    bool StringReader::CanRead() const {
        return _cursor + 1 <= _str.length();
    }

    char StringReader::Peek() const {
        return _str[_cursor];
    }

    size_t StringReader::GetCursor() const {
        return _cursor;
    }

    void StringReader::SetCursor(size_t cursor) {
        _cursor = std::min(cursor, _str.length());
    }

    bool StringReader::StartsWith(std::string_view str) const {
        return _str.find(str) == _cursor;
    }

    std::string_view StringReader::GetRemaining() const {
        return &_str[_cursor];
    }

    Errorable<std::string> StringReader::ReadStringUntil(char terminator)
    {
        bool escaped = false;

        std::stringstream ss;
        while (CanRead())
        {
            char c = _str[_cursor];
            ++_cursor;

            if (escaped)
            {
                if (c == terminator || c == Escape)
                {
                    ss << c;
                    escaped = false;
                }
                else
                {
                    _cursor -= 1;
                    return Errorable<std::string>::MakeError("Invalid escape sequence");
                }
            }
            else if (c == Escape)
            {
                escaped = true;
            }
            else if (c == terminator)
                return Errorable<std::string>::MakeSuccess(ss.str());
            else
                ss << c;
        }

        return Errorable<std::string>::MakeError("Missing closing quote");
    }

    template <typename T, typename ... Args, typename>
    auto StringReader::Read(Args&&... args) -> Errorable<typename ArgumentType<T>::type>
    {
        size_t startCursor = _cursor;

        static auto isAllowedNumber = [](char c) {
            return (c >= '0' && c <= '9') || c == '.' || c == '-' || c == '+';
        };

        while (CanRead() && isAllowedNumber(Peek()))
            Skip();

        if (_cursor == startCursor)
            return Errorable<typename ArgumentType<T>::type>::MakeError("Cannot read");

        typename ArgumentType<T>::type value;
        auto result = std::from_chars(&_str[startCursor], _str.data() + _str.size(),
                                      value,
                                      std::forward<Args>(args)...);
        if (result.ec != std::errc{})
        {
            _cursor = startCursor;

            return Errorable<typename ArgumentType<T>::type>::MakeError("Cannot parse");
        }
        else
            _cursor = result.ptr - _str.data();

        return Errorable<typename ArgumentType<T>::type>::MakeSuccess(std::move(value));
    }

    // ^^^ StringReader / LiteralTreeNode<S, R> vvv
        
    template <typename S, typename R>
    LiteralTreeNode<S, R>::LiteralTreeNode(LiteralTreeNode&& other) noexcept: _literal(std::move(other._literal)),
                                                                              _children(std::move(other._children)),
                                                                              _condition(std::move(other._condition)),
                                                                              _handler(std::move(other._handler))
    {
    }

    template <typename S, typename R>
    LiteralTreeNode<S, R>::LiteralTreeNode(std::string_view literal): _literal(literal)
    {
    }

    template <typename S, typename R>
    ParseResult<S, R> LiteralTreeNode<S, R>::Parse(StringReader& reader) const noexcept
    {
        size_t startIndex = reader.GetCursor();

        if (!reader.StartsWith(_literal))
            return { "Cannot parse input" };

        reader.Skip(_literal.length());
        if (reader.Peek() != ' ')
        {
            reader.SetCursor(startIndex);

            return { "Cannot parse input" };
        }

        for (auto itr = _children.begin(); itr != _children.end(); ++itr)
        {
            startIndex = reader.GetCursor();
            reader.Skip(); // Skip the space

            auto result = (*itr).Parse(reader);
            if (!result)
                reader.SetCursor(startIndex);
            else
                return result;
        }

        if (_condition == nullptr)
            return { "Unable to parse command" };

        return _condition(reader);
    }

    template <typename S, typename R>
    LiteralTreeNode<S, R>& LiteralTreeNode<S, R>::Then(std::string const& literal)
    {
        return _children.emplace_back(literal);
    }

    template <typename S, typename R>
    template <typename F>
    LiteralTreeNode<S, R>& LiteralTreeNode<S, R>::Then(std::string const& literal, F&& fn)
    {
        return Then(literal).Executes(fn);
    }

    template <typename S, typename R>
    R LiteralTreeNode<S, R>::Execute(S const& source, StringReader& input) const {
        return _handler(source, input);
    }

    template <typename S, typename R>
    template <typename F>
    LiteralTreeNode<S, R>& LiteralTreeNode<S, R>::Executes(F fn)
    {
        static_assert(
            boost::callable_traits::is_noexcept_v<F>,
            "Command handlers must be noexcept"
        );

        using args_t = boost::callable_traits::args_t<F>;

        if (!_children.empty())
        {
            static_assert(!std::is_same_v<Word, std::tuple_element_t<0, args_t>>,
                "Cannot register a command handler taking a string as first argument when there are subcommands");

            static_assert(!std::is_same_v<std::string, std::tuple_element_t<0, args_t>>,
                "Cannot register a command handler taking a string as first argument when there are subcommands");
        }

        using args_t = boost::callable_traits::args_t<F>;

        _condition = [this](StringReader& reader) -> ParseResult<S, R>
        {
            size_t startIndex = reader.GetCursor();

            try
            {
                auto validationResult = _ValidateParameters(reader, static_cast<args_t*>(nullptr));
                if (!validationResult.has_value())
                {
                    reader.SetCursor(startIndex);

                    return {*this, reader};
                }

                reader.SetCursor(startIndex);
                return {validationResult.value()};
            }
            catch (std::exception const& ex)
            {
                reader.SetCursor(startIndex);
                return { fmt::format("Unable to parse argument: {}", ex.what()) };
            }
        };

        _handler = [fn](S const& source, StringReader& reader)
        {
            return _ApplyCall(fn, source, reader, static_cast<args_t*>(nullptr));
        };

        return *this;
    }

    template <typename S, typename R>
    template <typename F, typename ... Ts>
    R LiteralTreeNode<S, R>::_ApplyCall(F fn, S const& source, StringReader& reader, std::tuple<Ts...>* tpl)
    {
        // Syntax matters here. 
        return std::apply(fn, std::tuple { _ParameterExtractor<S, Ts>::Extract(reader, source)... });
    }

    template <typename S, typename R>
    template <typename ... Ts>
    auto LiteralTreeNode<S, R>::_ValidateParameters(StringReader& reader, std::tuple<Ts...>* tpl)
    {
        return _ValidateParameters<Ts...>(reader, 0, sizeof...(Ts));
    }

    template <typename S, typename R>
    template <typename T>
    std::optional<T> LiteralTreeNode<S, R>::_SafeExtractParameter(S const& source, StringReader& reader, size_t& itr,
                                                                  size_t count)
    {
        if (itr == count || reader.Peek() != ' ')
            return std::nullopt;

        ++itr;
        return _ParameterExtractor<S, T>::Extract(reader, source);
    }

    template <typename S, typename R>
    template <typename T, typename ... Ts>
    std::optional<std::string> LiteralTreeNode<S, R>::_ValidateParameters(StringReader& reader, size_t itr, size_t count)
    {
        if (itr == count)
            return "Unable to read";

        if constexpr (!ArgumentType<T>::optional)
        {
            if (!reader.CanRead() || reader.Peek() != ' ')
                return "Unable to read: missing required parameter";
        }

        if constexpr (ArgumentType<T>::optional && sizeof...(Ts) > 0)
            static_assert(ArgumentType<std::tuple_element_t<0, std::tuple<Ts...>>>::optional, "Optional parameters must all be the last parameters of a command handler.");

        if constexpr (!std::is_same_v<std::decay_t<T>, S>)
        {
            reader.Skip();
            auto result = ArgumentType<std::decay_t<T>>::Read(reader);
            if (!result.Success)
                return result.Error;
        }

        if constexpr (sizeof...(Ts) > 0)
            return _ValidateParameters<Ts...>(reader, itr + 1, count);

        if (!reader.CanRead())
            return std::nullopt;

        return "Mismatched parameter count";
    }

    // ^^^ LiteralTreeNode<S, R> / ParseResult<S, R> vvv
    
    template <typename S, typename R>
    ParseResult<S, R>::ParseResult(LiteralTreeNode<S, R> const& node, StringReader const& input): _node(std::cref(node)),
        _input(input), _errorString(std::nullopt)
    {
    }

    template <typename S, typename R>
    template <typename ... Ts>
    ParseResult<S, R>::ParseResult(std::string const& f, Ts&&... args):
        _errorString(fmt::format(f, std::forward<Ts>(args)...)), _input(std::nullopt), _node(std::nullopt)
    {
    }

    template <typename S, typename R>
    R ParseResult<S, R>::Execute(S const& source) {
        if (!_errorString.has_value())
            return _node->get().Execute(source, _input.value());

        throw std::logic_error("Could not parse");
    }

    template <typename S, typename R>
    ParseResult<S, R>::operator bool() const
    {
        return !_errorString.has_value();
    }

    // ParseResult<S, R> / CommandDispatcher<S, R> vvv

    template <typename S, typename R>
    LiteralTreeNode<S, R>& CommandDispatcher<S, R>::Then(std::string_view literal) {
        auto [itr, success] = _children.emplace(std::piecewise_construct,
            std::forward_as_tuple(literal),
            std::forward_as_tuple(literal));

        if (success)
            return itr->second;

        throw std::logic_error("unable to store");
    }

    template <typename S, typename R> 
    template <typename F>
    LiteralTreeNode<S, R>& CommandDispatcher<S, R>::Then(std::string_view literal, F&& fn)
    {
        return Then(literal).Executes(fn);
    }

    template <typename S, typename R>
    R CommandDispatcher<S, R>::Parse(S const& source, std::string_view string) {
        StringReader reader { string  };
        return Parse(source, reader);
    }

    template <typename S, typename R>
    R CommandDispatcher<S, R>::Parse(S const& source, StringReader& reader) {
        if (_children.empty())
            throw std::logic_error("No registered command");

        for (auto itr = _children.begin(); itr != _children.end(); ++itr)
        {
            auto result = itr->second.Parse(reader);
            if (result)
                return result.Execute(source);
        }

        throw std::logic_error("Cannot parse command");
    }

    // ^^^ CommandDispatcher<S, R> / ArgumentType<T> vvv

    template <>
    auto ArgumentType<uint32_t>::Read(StringReader& reader) -> Errorable<type> {
        return reader.Read<uint32_t>(10);
    }
    template <>
    auto ArgumentType<int32_t>::Read(StringReader& reader) -> Errorable<type> {
        return reader.Read<int32_t>(10);
    }
    template <>
    auto ArgumentType<uint64_t>::Read(StringReader& reader) -> Errorable<type> {
        return reader.Read<uint64_t>(10);
    }
    template <>
    auto ArgumentType<int64_t>::Read(StringReader& reader) -> Errorable<type> {
        return reader.Read<int64_t>(10);
    }
    template <>
    auto ArgumentType<uint16_t>::Read(StringReader& reader) -> Errorable<type> {
        return reader.Read<uint16_t>(10);
    }
    template <>
    auto ArgumentType<int16_t>::Read(StringReader& reader) -> Errorable<type> {
        return reader.Read<int16_t>(10);
    }
    template <>
    auto ArgumentType<uint8_t>::Read(StringReader& reader) -> Errorable<type> {
        return reader.Read<uint8_t>(10);
    }
    template <>
    auto ArgumentType<int8_t>::Read(StringReader& reader) -> Errorable<type> {
        return reader.Read<int8_t>(10);
    }
    template <>
    auto ArgumentType<float>::Read(StringReader& reader) -> Errorable<type> {
        return reader.Read<float>(std::chars_format::general | std::chars_format::scientific | std::chars_format::fixed);
    }
    template <>
    auto ArgumentType<double>::Read(StringReader& reader) -> Errorable<type> {
        return reader.Read<double>(std::chars_format::general | std::chars_format::scientific | std::chars_format::fixed);
    }
    template <>
    auto ArgumentType<std::string>::Read(StringReader& reader) -> Errorable<type> {
        return reader.ReadString();
    }
    template <>
    auto ArgumentType<Word>::Read(StringReader& reader) -> Errorable<type> {
        return reader.ReadUnquotedString()
            .Convert<Word>([](std::string const& value) {
                return Word { value };
            });
    }
    template <>
    auto ArgumentType<QuotedString>::Read(StringReader& reader) -> Errorable<type> {
        return reader.ReadQuotedString()
            .Convert<QuotedString>([](std::string const& value) {
                return QuotedString { value };
            });
    }
    template <>
    auto ArgumentType<GreedyString>::Read(StringReader& reader) -> Errorable<type> {
        return Errorable<std::string>::MakeSuccess(std::string { reader.GetRemaining() })
            .Convert<GreedyString>([](std::string const& value) {
                return GreedyString { value };
            });
    }

    template <typename T>
    auto ArgumentType<std::optional<T>>::Read(StringReader& reader) -> Errorable<type>
    {
        // Confusing? Since this is for optional parameters parsing, this never fails
        auto result = ArgumentType<T>::Read(reader);
        if (result.Success)
            return Errorable<std::optional<T>>::MakeSuccess(result.Value);

        return Errorable<std::optional<T>>::MakeSuccess(std::nullopt);
    }
}
