/* 
 * This file is part of the snippetspp distribution (https://github.com/Warpten/snippetspp).
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


#include <fmt/format.h>

#include <array>
#include <cassert>
#include <charconv>
#include <functional>
#include <string_view>
#include <type_traits>
#include <unordered_map>

#include <boost/algorithm/string.hpp>
#include <boost/callable_traits/args.hpp>
#include <boost/callable_traits/is_noexcept.hpp>

static_assert(__cplusplus >= 201703L, "Brigadier only supports C++17 and upwards.");

#if __cplusplus >= 202002L
namespace Brigadier::Details
{
    template <typename T>
    using remove_cvref = std::remove_cvref<T>;

    template< class T >
    using remove_cvref_t = typename remove_cvref<T>::type;
}
#else
namespace Brigadier
{
    namespace Details
    {
        template <typename T>
        struct remove_cvref {
            typedef std::remove_cv_t<std::remove_reference_t<T>> type;
        };

        template< class T >
        using remove_cvref_t = typename remove_cvref<T>::type;
    }
}
#endif

namespace Brigadier {
    namespace Details {
        template <bool B, bool T, bool F>
        struct conditional_v;
        
        template <bool T, bool F>
        struct conditional_v<true, T, F> { constexpr static const bool value = T; };

        template <bool T, bool F>
        struct conditional_v<false, T, F> { constexpr static const bool value = F; };

        template <typename T> struct TupleSize;
        template <typename... Ts> struct TupleSize<std::tuple<Ts...>> { constexpr static const size_t value = sizeof...(Ts); };
        template <> struct TupleSize<std::tuple<>> { constexpr static const size_t value = 0; };

        template <typename T, typename... Ts>
        struct ValidateTree {
            constexpr static const bool value = conditional_v<
                sizeof...(Ts) == 0, 
                true, 
                (std::is_same_v<T, Ts> && ...)
            >::value;
        };

        template <typename T>
        struct is_optional : std::false_type { };

        template <typename T>
        struct is_optional<std::optional<T>> : std::true_type { };

        template <typename T>
        constexpr static const bool is_optional_v = is_optional<T>::value;

        template <
            auto Start,
            auto End,
            auto Inc,
            typename F,
            typename... Args
        >
        constexpr auto constexpr_for(F&& f, Args&&... args)
        {
            if constexpr (Start < End)
            {
                auto r { f(std::integral_constant<decltype(Start), Start> { }, std::forward<Args&&>(args)...) };
                using return_type = decltype(r);

                if constexpr (std::is_convertible_v<return_type, bool>) {
                    if (!r)
                        return false;
                    
                    return constexpr_for<Start + Inc, End, Inc, F>(std::forward<F&&>(f), std::forward<Args&&>(args)...);
                } else {
                    return constexpr_for<Start + Inc, End, Inc, F>(std::forward<F&&>(f), std::forward<Args&&>(args)...);
                }
            }

            return true; // Shut up the compiler
        }

        template <typename T> struct is_reference_wrapper : std::false_type { };
        template <typename T> struct is_reference_wrapper<std::reference_wrapper<T>> : std::true_type { };

        template <typename T>
        constexpr static const bool is_reference_wrapper_v = is_reference_wrapper<T>::value;
    }

    template <typename T>
    struct Errorable final {
        template <typename... Ts>
        static Errorable<T> MakeError(std::string_view fmt, Ts&&... args) noexcept {
            return Errorable<T> { fmt::format(fmt, std::forward<Ts&&>(args)...) };
        }

        static Errorable<T> MakeSuccess(T&& value) noexcept {
            return Errorable<T> { std::move(value) };
        }

        constexpr operator bool() const { return _value.has_value(); }
        constexpr operator T const& () const { return _value.value(); }

        template <typename U>
        Errorable<U> Map(std::function<U(T const&)> fn) const noexcept {
            if (!_value.has_value())
                return Errorable<U> { _error.value() };
            return Errorable<U> { std::move(fn(_value.value())) };
        }

        constexpr T const& value() const noexcept { return _value.value(); }
        constexpr T const& value() noexcept { return _value.value(); }

        Errorable(std::string err) noexcept
            : _value(std::nullopt), _error(std::move(err))
        { }

        Errorable(T&& val) noexcept
            : _value(std::in_place, val), _error(std::nullopt)
        {

        }

        Errorable(Errorable<T> const&) = delete;
        Errorable(Errorable<T>&&) noexcept = default;

    private:
        std::optional<T> _value;
        std::optional<std::string> _error;
    };

    template <> struct Errorable<std::string> final {
        operator bool() const { return _success; }
        operator std::string const& () const { return _value; }

        std::string const& value() const noexcept { return _value; }
        std::string const& value() noexcept { return _value; }

        template <typename... Ts>
        static Errorable<std::string> MakeError(std::string_view fmt, Ts&&... args) noexcept {
            return Errorable<std::string> { false, fmt::format(fmt, std::forward<Ts&&>(args)...) };
        }

        static Errorable<std::string> MakeSuccess(std::string&& value) noexcept {
            return Errorable<std::string> { true, std::move(value) };
        }

        Errorable(bool success, std::string&& value) noexcept
            : _value(value), _success(success)
        {

        }

        Errorable(Errorable<std::string> const&) = delete;
        Errorable(Errorable<std::string>&&) noexcept = default;

    private:
        std::string _value;
        bool _success;
    };

    template <typename T, typename... Ts>
    struct Tree {
        T _value;
        std::tuple<Ts...> _children;

        template<class V, class... Args>
        explicit constexpr Tree(V&& value, Args&&... children) noexcept
            : _value(std::forward<V>(value)), _children(std::forward<Args>(children)...)
        { }

        explicit constexpr Tree(T&& value, Ts&&... children) noexcept
            : _value(std::forward<T&&>(value)), _children(std::forward<Ts>(children)...)
        { }

        template <typename... Us>
        constexpr Tree<T, Ts..., Us...> And(Us&&... children) {
            return std::make_from_tuple<Tree<T, Ts..., Us...>>(
                std::tuple {
                    _value,
                    std::tuple_cat(_children, std::tuple { std::forward<Us&&>(children)... })
                }
            );
        }
    };

    inline constexpr struct TEmpty { } Empty;

    template<class ValueT, class... Children>
    Tree(ValueT&&, Children&&... c) -> Tree<Details::remove_cvref_t<ValueT>, Details::remove_cvref_t<Children>...>;

    template <typename Fn> struct CommandNode;
    struct BareNode;

    template <typename S>
    struct ParseResult;

    struct String {
        String(std::string_view sv) noexcept : _str(sv) { }

        operator const std::string() const noexcept { return _str; }
        const std::string& AsString() const noexcept { return _str; }

    private:
        std::string const _str;
    };

    enum class ValidationResult : uint8_t {
        Failure,
        Children,
        Execute
    };

    struct QuotedString final : String { using String::String; };
    struct Word final : String { using String::String; };

    struct StringReader final {
        constexpr StringReader(std::string_view content) noexcept : _content(content) {

        }

        void skip(size_t count = 1) noexcept { _cursor += count; }

        constexpr operator bool() const noexcept { return _cursor <= _content.length(); }
        constexpr const std::string_view view() const noexcept { return _content.substr(_cursor); }

        constexpr size_t cursor() const noexcept { return _cursor; }
        constexpr void cursor(size_t cursor) noexcept { _cursor = cursor; }

    private:
        std::string_view _content;
        size_t _cursor = 0;
    };

    template <typename S, typename T, typename Enable = void>
    struct _ParameterExtractor;

    template <typename S, typename T>
    struct _ParameterExtractor<S, T, std::enable_if_t<std::is_integral_v<T>>> {
        template <typename Fn>
        static auto _Extract(CommandNode<Fn> const&, S&, StringReader& reader) noexcept {
            T value;
            auto result = std::from_chars(
                reader.view().data(),
                reader.view().data() + reader.view().size(),
                value, 
                10);

            bool success = result.ec == std::errc { };
            if (success) {
                reader.skip(result.ptr - reader.view().data());
                return Errorable<T>::MakeSuccess(std::move(value));
            }

            return Errorable<T>::MakeError("Expected integer at offset {}", reader.cursor());
        }
    };

    template <typename S, typename T>
    struct _ParameterExtractor<S, T, std::enable_if_t<std::is_floating_point_v<T>>> {
        template <typename Fn>
        static auto _Extract(CommandNode<Fn> const&, S&, StringReader& reader) noexcept {
            T value;
            auto result = std::from_chars(
                reader.view().data(),
                reader.view().data() + reader.view().size(),
                value,
                std::chars_format::general | std::chars_format::scientific | std::chars_format::fixed
            );

            bool success = result.ec == std::errc { };
            if (success) {
                reader.skip(result.ptr - reader.view().data());
                return Errorable<T>::MakeSuccess(std::move(value));
            }

            return Errorable<T>::MakeError("Expected decimal at offset {}", reader.cursor());
        }
    };

    template <typename S, typename Fn>
    struct _ParameterExtractor<S, CommandNode<Fn> const&, void> {
        template <typename F = Fn>
        constexpr static auto _Extract(CommandNode<F> const& node, S&, StringReader&) noexcept {
            return std::cref(node);
        }
    };

    template <typename S, typename T>
    struct _ParameterExtractor<S, std::optional<T>, void>
    {
        template <typename F>
        static auto _Extract(CommandNode<F> const& node, S& source, StringReader& reader) noexcept
        {
            // TODO: optimize all of this
            size_t cursor = reader.cursor();

            auto ret = _ParameterExtractor<S, T>::_Extract(node, source, reader);
            if (!ret) {
                reader.cursor(cursor - 1);

                return Errorable<std::optional<T>>::MakeSuccess(std::nullopt);
            }

            reader.cursor(cursor - 1);
            return ret.template Map<std::optional<T>>([](auto value) noexcept {
                return std::optional<T> { value };
            });
        }
    };

    template <typename S>
    struct _ParameterExtractor<S, QuotedString, void> {
        template <typename Fn>
        static auto _Extract(CommandNode<Fn> const&, S&, StringReader& reader) noexcept {
            auto&& view = reader.view();
            bool success = (view[0] == '"' || view[0] == '\'');
            if (!success)
                return Errorable<QuotedString>::MakeError("Expected opening quote at offset {}", reader.cursor());

            auto delim = view[0];
            auto searchIndex = 1;

            bool escapes = false;

            while (true) {
                auto pos = view.find(delim, searchIndex);
                success = pos != std::string_view::npos;
                if (!success)
                    return Errorable<QuotedString>::MakeError("Expected closing quote");

                // Escaped, keep searching
                if (view[pos - 1] == '\\' && view[pos - 2] != '\\') {
                    escapes = true;
                    searchIndex = pos + 1;
                    continue;
                } else {
                    // Found end quote
                    searchIndex = pos;
                    break;
                }
            }

            std::string data { view.substr(1, searchIndex - 1) };
            reader.skip(searchIndex);

            // Remove escape sequences, keep quotes
            if (escapes)
                boost::erase_all(data, "\\");

            return Errorable<QuotedString>::MakeSuccess(QuotedString { std::move(data) });
        }
    };

    template <typename S>
    struct _ParameterExtractor<S, S&, void> {
        template <typename Fn>
        constexpr static auto _Extract(CommandNode<Fn> const&, S& source, StringReader& reader) noexcept {
            reader.cursor(reader.cursor() - 1);
            return std::ref(source);
        }
    };

    struct BareNode final {
        constexpr BareNode(std::string_view const literal) : _literal(literal) { }

        constexpr BareNode(BareNode const&) = default;
        constexpr BareNode(BareNode&&) = default;

        constexpr ValidationResult Validate(StringReader& reader) const noexcept {
            bool success = reader.view().find(_literal) == 0;
            
            if (success) {
                reader.skip(_literal.length());
                return ValidationResult::Children;
            }

            return ValidationResult::Failure;
        }

        template <typename... Ts>
        constexpr Tree<BareNode, Ts...> Then(Ts&&... parameters) const {
            return Tree { *this, std::forward<Ts&&>(parameters)... };
        }


    private:
        std::string_view _literal;
    };

    template <typename Fn>
    struct CommandNode final {
        constexpr CommandNode(std::string_view literal, Fn&& fn) noexcept : _literal(literal), _fn(fn) {
            static_assert(boost::callable_traits::is_noexcept_v<Fn>, "Function handlers must be noexcept.");
        }

        constexpr CommandNode(CommandNode<Fn> const&) = default;
        constexpr CommandNode(CommandNode<Fn>&&) noexcept = default;

        //> Returns true if execution succeeded, false otherwise.
        template <typename S>
        constexpr auto TryExecute(StringReader& reader, S& source) const noexcept
            -> bool
        {
            if (reader.view().find(_literal) != 0)
                return false;
            
            size_t cursor = reader.cursor();
            reader.cursor(cursor + _literal.length());

            using args_t = boost::callable_traits::args_t<Fn>;
            bool success = true;
            auto args {
                _ExtractParameters<S>(source, reader, static_cast<args_t*>(nullptr)) 
            };

            if (!_Validate(args)) {
                reader.cursor(cursor);

                return false;
            }

            std::apply(_fn, std::move(args));
            return true;
        }

    private:
        std::string_view _literal;
        Fn _fn;

        template <typename S, typename... Ts>
        constexpr auto _ExtractParameters(S& source, StringReader& reader, std::tuple<Ts...>* tpl = nullptr) const noexcept {
            return std::tuple { 
                _ExtractParameter<S, Ts>(source, reader)...
            };
        }

        template <typename S, typename T>
        constexpr auto _ExtractParameter(S& source, StringReader& reader) const noexcept {
            reader.skip();
            return _ParameterExtractor<S, T>::_Extract(*this, source, reader);
        }

        template <typename... Ts>
        constexpr static bool _Validate(std::tuple<Ts...> const& tpl) noexcept
        {
            return Details::constexpr_for<0u, sizeof...(Ts), 1>([](auto i, auto&& tpl) {
                using element_type = std::tuple_element_t<i, std::tuple<Ts...>>;

                if constexpr (!Details::is_reference_wrapper_v<element_type>)
                    return !!std::get<i>(tpl);
                
                return true;
            }, tpl);
        }
    };

    template <typename S>
    struct TreeParser final {
        template <typename T, typename... Ts>
        constexpr static auto Parse(StringReader& reader, Tree<T, Ts...> const root, S& source) noexcept
            -> bool
        {
            if constexpr (std::is_same_v<T, BareNode>) {
                ValidationResult rootResult { root._value.Validate(reader) };

                switch (rootResult) {
                    case ValidationResult::Failure:
                        break;
                    case ValidationResult::Children:
                    {
                        // Iterate each child.
                        // Just call Parse on each child in the tuple `root._children`,
                        // stopping on the first one that returns a non-null std::function.
                        if constexpr (sizeof...(Ts) == 0)
                            break;
                        else {
                            // Skip space to next child.
                            reader.skip(1);

                            return ForEachNode<0u>(root._children, [&source](auto node, auto reader) {
                                return Parse(reader, node, source);
                            }, [](auto result) -> bool {
                                return result != false;
                            }, std::forward<StringReader&>(reader));
                        }
                    }
                    case ValidationResult::Execute:
                        assert(false && "unreachable");
                        break;
                }

                return false;
            } else { // Implied CommandNode<Fn>, Fn hidden here.
                static_assert(Details::ValidateTree<TEmpty, Ts...>::value, "Malformed tree: command nodes cannot have any children.");

                return Parse(reader, root._value, source);
            }
        }

        template <typename Fn>
        constexpr static auto Parse(StringReader& reader, CommandNode<Fn> const& root, S& source) noexcept
            -> bool
        {
            return root.template TryExecute<S>(reader, source);
        }

        template <
            size_t I,
            typename Tuple, 
            typename Fn,
            typename Condition,
            typename... Args>
        constexpr static auto ForEachNode(
            Tuple&& tuple,
            Fn&& fn,
            Condition&& condition,
            Args&&... args) noexcept
        {
            if constexpr (Details::TupleSize<std::decay_t<Tuple>>::value > 0) {
                auto result { fn(std::get<I>(tuple), std::forward<Args&&>(args)...) };
                if constexpr (I + 1 == std::tuple_size<std::decay_t<Tuple>>::value)
                    return result;
                else {
                    if (condition(result))
                        return result;
                    else
                        return ForEachNode<I + 1>(std::forward<Tuple&&>(tuple),
                            std::forward<Fn&&>(fn),
                            std::forward<Condition&&>(condition),
                            std::forward<Args&&>(args)...
                        );
                }
            }
        }
    };

    template <typename Fn>
    constexpr static CommandNode<Fn> Command(std::string_view literal, Fn&& fn) noexcept {
        return CommandNode<Fn> { literal, std::forward<Fn&&>(fn) };
    }

    constexpr static BareNode Node(std::string_view literal) noexcept {
        return BareNode { literal };
    }
}
