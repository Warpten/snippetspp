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

static_assert(__cplusplus >= 201703L, "Brigadier only supports C++17 and upwards.");

#if __has_include(<format>) && __cplusplus >= 202002L && defined(__cpp_lib_format)
# include <format>
# define BRIGADIER_FORMAT_NAMESPACE std
#elif __has_include(<fmt/format.h>)
# include <fmt/format.h>
# define BRIGADIER_FORMAT_NAMESPACE fmt
#else
static_assert(false, "Brigadier requires std::format or fmt::format");
#endif

#include <array>
#include <cassert>
#include <charconv>
#include <optional>
#include <functional>
#include <string_view>
#include <type_traits>
#include <unordered_map>

#include <boost/algorithm/string.hpp>
#include <boost/callable_traits/args.hpp>
#include <boost/callable_traits/is_noexcept.hpp>

#if __cplusplus >= 202002L
namespace Brigadier::Details
{
    template <typename T>
    using remove_cvref = std::remove_cvref<T>;

    template< class T >
    using remove_cvref_t = typename remove_cvref<T>::type;
}
#else
namespace Brigadier::Details
{
    template <typename T>
    struct remove_cvref {
        typedef std::remove_cv_t<std::remove_reference_t<T>> type;
    };

    template< class T >
    using remove_cvref_t = typename remove_cvref<T>::type;
}
#endif

namespace Brigadier {
    template <typename T, typename Enable = void>
    struct _ParameterExtractor;

    namespace Details {
        // ---- TupleSize ----
        // Like std::tuple_size but handles empty tuples properly
        namespace {
            template <typename T> struct TupleSize;
            template <typename... Ts> struct TupleSize<std::tuple<Ts...>> { constexpr static const size_t value = sizeof...(Ts); };
            template <> struct TupleSize<std::tuple<>> { constexpr static const size_t value = 0; };
        }
        
        // ---- ValidateTree ----
        namespace {
            // Ensures that a tree node has no children or that they are all a given T.
            template <typename T, typename... Ts>
            struct ValidateTree {
                constexpr static const bool value = sizeof...(Ts) == 0
                    ? true
                    : (std::is_same_v<T, Ts> && ...);
            };
        }

        // ---- is_optional
        namespace {
            template <typename T> struct is_optional : std::false_type { };
            template <typename T> struct is_optional<std::optional<T>> : std::true_type { };
            template <typename T>
            constexpr static const bool is_optional_v = is_optional<T>::value;
        }

        template <
            size_t I,
            typename Tuple, 
            typename Fn,
            typename Condition,
            typename... Args>
        constexpr static auto Iterate(
            Tuple&& tuple,
            Fn&& fn,
            Condition&& condition,
            Args&&... args)
        {
            if constexpr (Details::TupleSize<std::decay_t<Tuple>>::value > 0) {
                const auto result { fn(std::get<I>(tuple), std::forward<Args&&>(args)...) };
                if constexpr (I + 1 == std::tuple_size<std::decay_t<Tuple>>::value)
                    return result;
                else {
                    if (condition(result))
                        return result;
                    else
                        return Iterate<I + 1>(std::forward<Tuple&&>(tuple),
                            std::forward<Fn&&>(fn),
                            std::forward<Condition&&>(condition),
                            std::forward<Args&&>(args)...
                        );
                }
            }
        }

        // ---- std::is_reference_wrapper ----
        namespace {
            template <typename T> struct is_reference_wrapper : std::false_type { };
            template <typename T> struct is_reference_wrapper<std::reference_wrapper<T>> : std::true_type { };

            template <typename T>
            constexpr static const bool is_reference_wrapper_v = is_reference_wrapper<T>::value;
        }

        // ---- ParameterIsInput ----
        // Determines wether a parameter is parsed from the command text.
        namespace {
            template <typename T, typename Enable = void> struct ParameterIsInput {
                enum { value = false };
            };

                template <typename U>
                static std::true_type test(decltype(&_ParameterExtractor<U>::_Extract));
                static std::false_type test(...);

            template <typename T>
            struct ParameterIsInput<T, std::enable_if_t<decltype(test<T>(0))::value>>
            {
                enum { value = true };
            };
            template <typename T>
            struct ParameterIsInput<std::optional<T>> {
                enum { value = ParameterIsInput<T>::value };
            };
        }

        // ---- FilterHandlerParams ----
        // Filters types of a tuple based on a conditional, returning a reduced tuple.
        namespace {
            template <template <class...> typename Conditional, typename Tuple> struct FilterHandlerParams;
            template <template <class...> typename Conditional, typename T, typename... Ts>
            struct FilterHandlerParams<Conditional, std::tuple<T, Ts...>> {
                using type = std::conditional_t<Conditional<T>::value,
                    decltype(
                        std::tuple_cat(
                            std::declval<std::tuple<T>>(),
                            std::declval<typename FilterHandlerParams<Conditional, std::tuple<Ts...>>::type
                            >()
                        )
                    ),
                    typename FilterHandlerParams<Conditional, std::tuple<Ts...>>::type>;
            };
            template <template <class...> typename Conditional>
            struct FilterHandlerParams<Conditional, std::tuple<>> {
                using type = std::tuple<>;
            };
        }

        // ---- ValidateParameterInfo ----
        // Validations parameter information
        namespace {
            template <typename Fn, typename... Ts>
            struct ValidateParameterInfo
            {
                using source_tuple = typename FilterHandlerParams<
                    ParameterIsInput,
                    boost::callable_traits::args_t<Fn>
                >::type;
                using target_tuple = std::tuple<Ts...>;

                enum {
                    value = std::is_same_v<source_tuple, target_tuple>
                };
            };
        }
    }

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

    struct String {
        String() noexcept {
            _val = nullptr;
            _sz = 0;
            _owning = false;
        }

        explicit String(std::string_view sv) noexcept {
            _val = const_cast<char*>(sv.data());
            _sz = sv.length();
            _owning = false;
        }

        explicit String(const std::string& str) noexcept
        {
            _val = new char[str.length()];
            std::memcpy(_val, str.data(), str.length());
            _sz = str.length();
            _owning = true;
        }

        String(String&& other) noexcept
        {
            _val = other._val;
            _sz = other._sz;
            _owning = other._owning;

            other._val = nullptr;
            other._owning = false;
            other._sz = 0;
        }

        String(String const& other) noexcept
        {
            if (other._owning)
            {
                _val = new char[other._sz];
                _sz = other._sz;
                std::memcpy(_val, other._val, other._sz);
                _owning = true;
            }
            else
            {
                _val = other._val;
                _sz = other._sz;
                _owning = false;
            }
        }

        ~String() noexcept {
            if (_owning)
                delete[] _val;

            _val = nullptr;
            _sz = 0;
            _owning = false;
        }

        operator const std::string_view() const noexcept { return std::string_view { _val, _sz }; }

    private:
        char* _val;
        size_t _sz;
        bool _owning;
    };

    enum class ValidationResult : uint8_t {
        Failure,
        Children,
        Execute
    };

    struct QuotedString : String { using String::String; };
    struct Word : String { using String::String; };
    struct GreedyString : String { using String::String; };

    template <typename T>
    struct _ParameterExtractor<T, std::enable_if_t<std::is_integral_v<T>>> {
        constexpr static const T Default { };

        static auto _Extract(std::string_view& reader) noexcept
            -> std::optional<T>
        {
            T value;
            auto result = std::from_chars(
                reader.data(),
                reader.data() + reader.size(),
                value, 
                10);

            bool success = result.ec == std::errc { };
            if (success) {
                reader = result.ptr;
                return value;
            }

            return std::nullopt;
        }
    };

    template <typename T>
    struct _ParameterExtractor<T, std::enable_if_t<std::is_floating_point_v<T>>> {
        constexpr static const T Default { };

        static auto _Extract(std::string_view& reader) noexcept
            -> std::optional<T>
        {
            T value;
            auto result = std::from_chars(
                reader.data(),
                reader.data() + reader.size(),
                value,
                std::chars_format::general | std::chars_format::scientific | std::chars_format::fixed
            );

            bool success = result.ec == std::errc { };
            if (success) {
                reader = result.ptr;
                return value;
            }

            return std::nullopt;
        }
    };

    template <>
    struct _ParameterExtractor<Word, void> {
        static auto _Extract(std::string_view& reader) noexcept
            -> std::optional<Word>
        {
            auto pos = reader.find(' ', 0);
            if (pos == std::string_view::npos)
                return Word { reader };

            if (pos == 0)
                return std::nullopt;

            return Word { reader.substr(0, pos) };
        }
    };

    template <>
    struct _ParameterExtractor<GreedyString, void> {
        static auto _Extract(std::string_view& reader) noexcept
            -> std::optional<GreedyString>
        {
            return GreedyString { reader };
        }
    };

    template <>
    struct _ParameterExtractor<QuotedString, void> {
        static auto _Extract(std::string_view& reader) noexcept
            -> std::optional<QuotedString>
        {
            bool success = (reader[0] == '"' || reader[0] == '\'');
            if (!success)
                return std::nullopt;

            auto delim = reader[0];
            auto searchIndex = 1;

            bool escapes = false;

            while (true) {
                auto pos = reader.find(delim, searchIndex);
                success = pos != std::string_view::npos;
                if (!success)
                    return std::nullopt;

                // Escaped, keep searching
                if (reader[pos - 1] == '\\' && reader[pos - 2] != '\\') {
                    escapes = true;
                    searchIndex = pos + 1;
                    continue;
                } else {
                    // Found end quote
                    searchIndex = pos;
                    break;
                }
            }

            std::string_view data { reader.substr(1, searchIndex - 1) };
            reader = reader.substr(searchIndex + 1);

            // Remove escape sequences, keep quotes
            if (escapes)
            {
                std::string owningCopy { data };
                boost::erase_all(owningCopy, "\\");

                return QuotedString { std::move(owningCopy) };
            }

            return QuotedString { data };
        }
    };

    struct BareNode {
        constexpr BareNode(std::string_view const literal) noexcept : _literal(literal) { }

        constexpr BareNode(BareNode const&) = default;
        constexpr BareNode(BareNode&&) = default;

        constexpr ValidationResult Validate(std::string_view& reader) const noexcept {
            bool success = reader.find(_literal) == 0;
            
            if (success) {
                reader = reader.substr(_literal.length());
                return ValidationResult::Children;
            }

            return ValidationResult::Failure;
        }

        template <typename... Ts>
        constexpr Tree<BareNode, Ts...> Then(Ts&&... parameters) const noexcept {
            return Tree { *this, std::forward<Ts&&>(parameters)... };
        }


    private:
        std::string_view _literal;
    };

    template <typename Fn>
    struct CommandNode {
        constexpr CommandNode(std::string_view literal, Fn&& fn) noexcept : _literal(literal), _fn(fn) {
            static_assert(boost::callable_traits::is_noexcept_v<Fn>, "Function handlers must be noexcept.");
        }

        constexpr CommandNode(CommandNode<Fn> const&) = default;
        constexpr CommandNode(CommandNode<Fn>&&) noexcept = default;

        //> Returns true if execution succeeded, false otherwise.
        template <typename S>
        constexpr auto TryExecute(std::string_view const& reader, S& source) const noexcept
            -> bool
        {
            if (reader.find(_literal) != 0)
                return false;

            auto mutableReader = reader.substr(_literal.length());

            using args_t = boost::callable_traits::args_t<Fn>;

            bool success = true;
            auto args { _ExtractParameters<S>(source, success, mutableReader, static_cast<args_t*>(nullptr)) };

            if (success)
                std::apply(_fn, std::move(args));

            return success;
        }

    private:
        std::string_view _literal;
        Fn _fn;

        template <typename S, typename... Ts>
        constexpr auto _ExtractParameters(S& source, bool& success, std::string_view& reader, std::tuple<Ts...>* tpl = nullptr) const {
            return std::tuple {  _ExtractParameter<S, Ts>(source, reader, success)... };
        }

        template <typename S, typename T>
        constexpr auto _ExtractParameter(S& source, std::string_view& reader, bool& success) const noexcept {
            
            using decayed_type = std::decay_t<T>;

            if constexpr (std::is_same_v<T, S&>) {
                return std::ref(source);
            } else if constexpr (std::is_same_v<T, S const&>) {
                return std::cref(source);
            } else if constexpr (std::is_same_v<T, CommandNode<Fn>&>) {
                return std::ref(*this);
            } else if constexpr (std::is_same_v<T, CommandNode<Fn> const&>) {
                return std::cref(*this);
            } else if constexpr (Details::is_optional_v<T>) {
                static_assert(Details::ParameterIsInput<decayed_type>::value, "Missing implementation of _ParameterExtractor");

                using underlying_type = typename decayed_type::value_type;

                // If we already failed, fail fast
                if (success) {
                    std::string_view copy { reader.substr(1) };
                    auto result { _ParameterExtractor<underlying_type>::_Extract(copy) };
                    if (result.has_value())
                        reader = copy;

                    return result;
                }

                return std::optional<underlying_type> { };
            } else {
                // If we already failed, fail fast
                if (!success)
                    return decayed_type { };

                static_assert(Details::ParameterIsInput<decayed_type>::value, "Missing implementation of _ParameterExtractor");

                reader = reader.substr(1);
                auto result { _ParameterExtractor<decayed_type>::_Extract(reader) };
                if (result.has_value())
                    return result.value();

                success = false;
                return decayed_type { };
            }
        }
    };
    
    template <typename U>
    struct ParameterMeta {
        constexpr static const bool Required = !Details::is_optional_v<U>;
        const std::string_view Name;

        using type = U;
    };

    template <typename Fn, typename... Ts>
    struct DetailedCommandNode : CommandNode<Fn> {
        using expected_params_t = boost::callable_traits::args_t<Fn>;

        using Base = CommandNode<Fn>;

        constexpr DetailedCommandNode(std::string_view literal,
            Fn&& fn,
            Ts&&... parameters)
            noexcept
            : Base(literal, std::forward<Fn&&>(fn)), _parameters({ parameters... })
        {
            static_assert(Details::ValidateParameterInfo<Fn, typename Ts::type...>::value,
                "Parameter informations do not match the command.");
        }

        std::tuple<Ts...> _parameters;
    };


    template <typename S>
    struct TreeParser final {
        template <typename T>
        constexpr static auto Parse(std::string_view input, T const root, S& source) noexcept
            -> bool
        {
            auto mutableView { input };
            return _Parse(mutableView, root, source);
        }

    private:
        template <typename T, typename... Ts>
        constexpr static auto _Parse(std::string_view& reader, Tree<T, Ts...> const root, S& source) noexcept
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
                        // stopping on the first one that works.
                        if constexpr (sizeof...(Ts) == 0)
                            break;
                        else {
                            // Skip space to next child.
                            reader = reader.substr(1);

                            return Details::Iterate<0u>(root._children, [&source](auto node, auto reader) {
                                return _Parse(reader, node, source);
                            }, [](auto result) -> bool {
                                return result != false;
                            }, std::forward<std::string_view&>(reader));
                        }
                    }
                    case ValidationResult::Execute:
                        assert(false && "unreachable");
                        break;
                }

                return false;
            } else { // Implied CommandNode<Fn>, Fn hidden here.
                static_assert(Details::ValidateTree<TEmpty, Ts...>::value,
                    "Malformed tree: command nodes cannot have any children.");

                return _Parse(reader, root._value, source);
            }
        }

        template <typename Fn>
        constexpr static auto _Parse(std::string_view& reader, CommandNode<Fn> const& root, S& source) noexcept
            -> bool
        {
            return root.template TryExecute<S>(reader, source);
        }

        template <typename Fn, typename... Ts>
        constexpr static auto _Parse(std::string_view& reader, DetailedCommandNode<Fn, Ts...> const& root, S& source) noexcept
            -> bool
        {
            return root.template TryExecute<S>(reader, source);
        }
    };

    template <typename Fn>
    constexpr static CommandNode<Fn> Command(std::string_view literal, Fn&& fn) noexcept {
        return CommandNode<Fn> { literal, std::forward<Fn&&>(fn) };
    }

    template <typename Fn, typename... Ts>
    constexpr static DetailedCommandNode<Fn, Ts...> Command(std::string_view literal, Fn&& fn, Ts&&... params) noexcept {
        return DetailedCommandNode<Fn, Ts...> { literal, std::forward<Fn&&>(fn), std::forward<Ts&&>(params)... };
    }

    constexpr static BareNode Node(std::string_view literal) noexcept {
        return BareNode { literal };
    }
}
