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
    }

    template <typename T, typename... Ts>
    struct Tree {
        T _value;
        std::tuple<Ts...> _children;

        template<class V, class... Args>
        explicit constexpr Tree(V&& value, Args&&... children)
            : _value(std::forward<V>(value)), _children(std::forward<Args>(children)...)
        { }

        explicit constexpr Tree(T&& value, Ts&&... children)
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
        String(std::string_view sv) : _str(sv) { }

        operator const std::string() const { return _str; }
        const std::string& AsString() const { return _str; }

    private:
        std::string const _str;
    };

    enum class ValidationResult : uint8_t {
        Failure,
        Children,
        Execute
    };

    struct QuotedString : String { using String::String; };
    struct Word : String { using String::String; };

    struct StringReader
    {
        constexpr StringReader(std::string_view content) noexcept : _content(content) {

        }

        void Skip(size_t count = 1) { _cursor += count; }

        constexpr operator bool() const { return _cursor <= _content.length(); }
        constexpr const std::string_view view() const { return _content.substr(_cursor); }

        constexpr size_t cursor() const { return _cursor; }
        constexpr void cursor(size_t cursor) { _cursor = cursor; }

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
            assert(result.ec == std::errc { });
            
            reader.Skip(result.ptr - reader.view().data());
            return value;
        }

        static auto _TryExtract(StringReader& reader) noexcept {
            T value;
            auto result = std::from_chars(
                reader.view().data(),
                reader.view().data() + reader.view().size(),
                value, 
                10);

            if (result.ec != std::errc { })
                return false;
            
            reader.Skip(result.ptr - reader.view().data());
            return true;
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
            assert(result.ec == std::errc { });
            
            reader.Skip(result.ptr - reader.view().data());
            return value;
        }

        static auto _TryExtract(StringReader& reader) noexcept {
            T value;
            auto result = std::from_chars(
                reader.view().data(),
                reader.view().data() + reader.view().size(),
                value,
                std::chars_format::general | std::chars_format::scientific | std::chars_format::fixed
            );

            if (result.ec != std::errc { })
                return false;
            
            reader.Skip(result.ptr - reader.view().data());
            return true;
        }
    };

    template <typename S, typename Fn>
    struct _ParameterExtractor<S, CommandNode<Fn> const&, void> {
        template <typename F = Fn>
        static auto _Extract(CommandNode<F> const& node, S&, StringReader&) noexcept {
            return std::cref(node);
        }

        static auto _TryExtract(StringReader&) noexcept { return true; }
    };

    template <typename S>
    struct _ParameterExtractor<S, QuotedString, void> {
        template <typename Fn>
        static auto _Extract(CommandNode<Fn> const&, S&, StringReader& reader) noexcept {
            auto&& view = reader.view();
            assert(view[0] == '"' || view[0] == '\'');

            auto delim = view[0];
            auto searchIndex = 1;

            while (true) {
                auto pos = view.find(delim, searchIndex);
                assert(pos != std::string_view::npos);

                // Escaped, keep searching
                if (view[pos - 1] == '\\' && view[pos - 2] != '\\') {
                    searchIndex = pos + 1;
                    continue;
                } else {
                    // Found end quote
                    searchIndex = pos;
                    break;
                }
            }

            std::string data { view.substr(1, searchIndex - 1) };
            reader.Skip(data.length() + 1);

            // Remove escape sequences, keep quotes
            boost::erase_all(data, "\\");
            return QuotedString { data };
        }

        static auto _TryExtract(StringReader& reader) noexcept {
            auto&& view = reader.view();
            if (view[0] != '"' && view[0] != '\'')
                return false;

            auto delim = view[0];
            auto searchIndex = 1;
            while (true) {
                auto pos = view.find(delim, searchIndex);
                if (pos == std::string_view::npos)
                    return false; // Expected closing quote
                
                // Found escaped, ignore and move on
                if (view[pos - 1] == '\\' && view[pos - 2] != '\\') {
                    searchIndex = pos + 1;
                } else {
                    reader.Skip(pos - searchIndex + 1);
                    return true;
                } 
            }

            return false; // Expected closing quote
        }
    };

    template <typename S>
    struct _ParameterExtractor<S, S&, void> {
        template <typename Fn>
        static auto _Extract(CommandNode<Fn> const&, S& source, StringReader&) noexcept {
            return std::ref(source);
        }

        static auto _TryExtract(StringReader&) noexcept { return true; }
    };

    struct BareNode {
        constexpr BareNode(std::string_view const literal) : _literal(literal) { }

        constexpr BareNode(BareNode const&) = default;
        constexpr BareNode(BareNode&&) = default;

        constexpr ValidationResult Validate(StringReader& reader) const noexcept {
            size_t cursor = reader.cursor();
            bool success = reader.view().find(_literal) == 0;
            
            if (success) {
                reader.cursor(cursor + _literal.length());
                return ValidationResult::Children;
            }

            reader.cursor(cursor);
            return ValidationResult::Failure;
        }

        template <typename... Ts>
        constexpr Tree<BareNode, Ts...> Then(Ts&&... parameters) const
        {
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

        template <typename S>
        constexpr ValidationResult Validate(StringReader& reader) const noexcept {
            if (reader.view().find(_literal) != 0)
                return ValidationResult::Failure;
            
            size_t cursor = reader.cursor();

            reader.Skip(_literal.length());
            bool success = _TryValidateCall<S>(reader, static_cast<boost::callable_traits::args_t<Fn>*>(nullptr));
            
            if (success) {
                reader.cursor(cursor + _literal.length());
                return ValidationResult::Execute;
            }

            reader.cursor(cursor);
            return ValidationResult::Failure;
        }

        template <typename S>
        constexpr decltype(auto) Call(S& source, StringReader& reader) const noexcept {
            return _ApplyCall(source, reader, static_cast<boost::callable_traits::args_t<Fn>*>(nullptr));
        }

    private:
        std::string_view _literal;
        Fn _fn;

        template <typename S, typename... Ts>
        constexpr decltype(auto) _ApplyCall(S& source, StringReader& reader, std::tuple<Ts...>* tpl = nullptr) const {
            return std::apply(_fn, std::tuple { _ExtractParameter<S, Ts>(source, reader)... });
        };

        template <typename S, typename... Ts>
        constexpr static auto _TryValidateCall(StringReader& reader, std::tuple<Ts...>* tpl = nullptr) {
            return (_TryExtractParameter<S, Ts>(reader) && ...);
        }

        template <typename S, typename T>
        constexpr auto _ExtractParameter(S& source, StringReader& reader) const {
            reader.Skip();
            return _ParameterExtractor<S, T>::_Extract(*this, source, reader);
        }

        template <typename S, typename T>
        constexpr static auto _TryExtractParameter(StringReader& reader) {
            reader.Skip();
            return _ParameterExtractor<S, T>::_TryExtract(reader);
        }
    };

    template <typename S>
    struct ParseResult {
        std::function<void(S const&)> _execution;
    };

    template <typename S>
    struct TreeParser final {
        template <typename T, typename... Ts>
        constexpr static auto Parse(StringReader& reader,
            Tree<T, Ts...> const root) noexcept
            -> std::function<void(S&)>
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
                            reader.Skip(1);

                            return ForEachNode<0u>(root._children, [](auto node, auto reader) {
                                return Parse(reader, node);
                            }, [](auto delegate) -> bool {
                                return delegate != nullptr;
                            }, std::forward<StringReader&>(reader));
                        }
                    }
                    case ValidationResult::Execute:
                        assert(false && "unreachable");
                    break;
                }

                return nullptr;
            } else { // Implied CommandNode<Fn>, Fn hidden here.
                static_assert(Details::ValidateTree<TEmpty, Ts...>::value, "Malformed tree: command nodes cannot have any children.");

                ValidationResult rootResult { root._value.template Validate<S>(reader) };
                switch (rootResult) {
                    case ValidationResult::Execute:
                        return [reader = std::move(reader), node = root._value](S& source) mutable {
                            return node.Call(source, reader); // May return anything
                        };
                    case ValidationResult::Children:
                        assert(false && "Unreachable");
                        [[fallthrough]];
                    case ValidationResult::Failure:
                        [[fallthrough]];
                    default:
                        break;
                }

                return nullptr;
            }
        }

        template <typename Fn>
        constexpr static auto Parse(StringReader& reader,
            CommandNode<Fn> const& root) noexcept
            -> std::function<void(S&)>
        {
            ValidationResult rootResult { root.template Validate<S>(reader) };
            switch (rootResult) {
                case ValidationResult::Execute:
                    return [reader = std::move(reader), node = root](S& source) mutable {
                        return node.Call(source, reader); // May return anything
                    };
                case ValidationResult::Children:
                    assert(false && "Unreachable");
                    [[fallthrough]];
                case ValidationResult::Failure:
                    [[fallthrough]];
                default:
                    break;
            }

            return nullptr;
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
            Args&&... args)
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
    constexpr static CommandNode<Fn> Command(std::string_view literal, Fn&& fn) {
        return CommandNode<Fn> { literal, std::forward<Fn&&>(fn) };
    }

    constexpr static BareNode Node(std::string_view literal) {
        return BareNode { literal };
    }
}
