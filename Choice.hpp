/* 
 * This file is part of the snippetspp distribution (https://github.com/Warpten/snippetspp).
 * Copyright (c) 2022 Warpten.
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

#include <algorithm>
#include <cstdint>
#include <tuple>
#include <cassert>
#include <functional>
#include <limits>
#include <type_traits>
#include <variant>
#include <utility>

#include <shared/Traits.hpp> // https://github.com/Warpten/snippetspp/blob/main/shared/Traits.hpp

/**
 * A glorified union (sum type). It has some limitations compared to std::variant:
 *  1. It is not permitted to hold cv-qualified types.
 *  2. Default-construction is not supported.
 * 
 * Things left to do: 
 *  1. Proper exception handling at value construction.
 *  2. Copy construction.
 *  3. Move construction
 *  4. ???
 *
 * Example use:
 *   Choice::Of<Tag::A, uint32_t>::Or<Tag::B, float> instance { Tag::A };
 */
namespace Choice {
    template <auto K, typename T> struct Of;
    template <typename E, typename... Ts> class Union;

#define MAKE_LVALUE(X) std::add_lvalue_reference_t<typename X::value_type>

    struct Uninitialized { };

    namespace Utils {
        template<class>
        struct AlwaysFalse : std::false_type { };

        /***********************************************************/

        /**
         * Does the given tag correspond to the given type?
         */
        template <typename...> struct IsTagForType { };
        template <typename Tag, typename Type>
        struct IsTagForType<Tag, Type> : std::is_same<Type, typename Tag::value_type> { };

        /**
         * Obtains the type associated to a tag.
         */
        template <typename...> struct TypeOfTag { };
        template <auto E, typename T> struct TypeOfTag<Of<E, T>> { using type = T; };

        /***********************************************************/

        /**
         * Filter used to eliminate tags that represents trivially constructible types.
         */
        template <typename...> struct NotTriviallyConstructible;
        template <typename T>
        struct NotTriviallyConstructible<T>
            : std::negation<std::is_trivially_constructible<typename TypeOfTag<T>::type>> { };

        /**
         * Filter user to eliminate but for trivially destructible types.
         */
        template <typename...> struct NotTriviallyDestructible;
        template <typename T>
        struct NotTriviallyDestructible<T>
            : std::negation<std::is_trivially_destructible<typename TypeOfTag<T>::type>> { };

        /***********************************************************/

        /**
         * Tests wether invoking the given Callable with the given arguments is ill-formed.
         * Nitpick: Why does the STL have std::invoke, but std::is_invocable ?
         * TODO: This allows implicit casts. Is that an issue? It should probably be.
         */
        template <typename T, typename... Args>
        constexpr static const bool Invokable = std::is_invocable_v<T, Args...>;
        
        template <typename F, typename V, typename E>
        void DispatchInvoke(F&& fn, V& value, E sel) {
            if constexpr (Invokable<F, V> || Invokable<F, V&> || Invokable<F, V const&>) {
                std::invoke(fn, value);
            } else if constexpr (Invokable<F, V, E> || Invokable<F, V&, E> || Invokable<F, V const&, E>) {
                std::invoke(fn, value, sel);
            } else if constexpr (Invokable<F, E, V> || Invokable<F, E, V&> || Invokable<F, E, V const&>) {
                std::invoke(fn, sel, value);
            }
        }
    }

    /**
     * @targ E The enumeration type used to tag each member of the union.
     * @targ Ts Tags contained in this union.
     */
    template <typename E, typename... Ts>
    class Union final {
        using non_trivially_constructible_sequence = typename Traits::Filter<
            Utils::NotTriviallyConstructible,
            Ts...
        >::template Execute<>;
        using non_trivially_destructible_sequence = typename Traits::Filter<
            Utils::NotTriviallyDestructible,
            Ts...
        >::template Execute<>;
        
        template <typename... Tags>
        auto TryAllocate(E selection, Traits::TypeSequence<Tags...>)
            noexcept(noexcept((Tags::TryAllocate(std::declval<E>(), std::declval<std::byte*>()) || ...)))
        {
            return (Tags::TryAllocate(selection, _storage) || ...);
        }

        template <typename... Tags>
        auto TryFree(E selection, Traits::TypeSequence<Tags...>)
            noexcept(noexcept((Tags::TryFree(std::declval<E>(), std::declval<std::byte*>()) || ...)))
        {
            return (Tags::TryFree(selection, _storage) || ...);
        }

    public:
        static_assert(
            !std::is_convertible_v<E, int>&& std::is_enum_v<E>,
            "Unions can only accept strongly typed enumerations."
        );

        constexpr static const size_t alternative_count = sizeof...(Ts);

        using key_type = E;
        using value_types = std::tuple<typename Utils::TypeOfTag<Ts>::type...>; // TODO: TypeSequence?

        template <E K2, typename T2, typename = std::enable_if_t<((K2 != Ts::key) && ...)>>
        using Or = Union<E,
            Of<Ts::key, typename Utils::TypeOfTag<Ts>::type>...,
            Of<K2, T2>
        >;

        /**
         * Constructs the union in the uninitialized state.
         */
        constexpr explicit Union(Uninitialized) noexcept {
            // Magic compiler value slightly modified. Hopefully no one ever needs that specific tag.
            _selection = static_cast<E>(0xCDCDCDCF);
        }

#define NOEXCEPT_ALLOCATION_TEST   noexcept(TryAllocate(std::declval<E>(), std::declval<non_trivially_constructible_sequence>()))
#define NOEXCEPT_DEALLOCATION_TEST noexcept(TryFree(std::declval<E>(), std::declval<non_trivially_destructible_sequence>()))

        /**
         * Constructs an instance of the union holding the type associated with the selection.
         * If that type is trivially constructible, the value is zero-initialized. Otherwise,
         * its parameterless constructor is called.
         */
        constexpr explicit Union(E selection) noexcept(NOEXCEPT_ALLOCATION_TEST) {
            Assign(selection);
        }
        
        ~Union() noexcept(NOEXCEPT_DEALLOCATION_TEST) {
            Assign(Uninitialized { });
        }

        /**
         * Sets the union to the uninitialized state. Zero-initializes the union.
         */
        [[deprecated("Use Assign(Choice::Uninitialized { })."]] void Clear() noexcept(NOEXCEPT_DEALLOCATION_TEST) {
            Assign(Uninitialized{ });
        }

        /**
         * Sets the union to the uninitialized state. Zero-initializes the union.
         */
        void Assign(Uninitialized) noexcept(NOEXCEPT_DEALLOCATION_TEST) {
            TryFree(_selection, non_trivially_destructible_sequence{});
            _storage = { 0 };

            _selection = static_cast<E>(0xCDCDCDCF);
        }

        /**
         * Assigns the value associated with the enumeration. If the union is not currently in the
         * uninitialized state, destroys the previous value.
         */
        void Assign(E selection) noexcept(NOEXCEPT_DEALLOCATION_TEST && NOEXCEPT_ALLOCATION_TEST) {
            assert(((selection == Ts::key) || ...) && "Attempt to initialize an union with an enumeration with no known type associated to it.");

            if (IsInitialized())
                Assign(Uninitialized{ });

            TryAllocate(selection, non_trivially_constructible_sequence{});
            _selection = selection;
        }

#undef NOEXCEPT_ALLOCATION_TEST
#undef NOEXCEPT_DEALLOCATION_TEST

        /**
         * Applies the provided Callables to the union.
         * Callables can have one of the following signatures:
         *  - void(V value)
         *  - void(V value, E tag)
         * V can be either an lvalue, an lvalue reference, or a const lvalue reference.
         */
        template <typename... Vs>
        void Visit(Vs... fns) {
            // TODO: Check proper indices for each visitor (we can have (E, T) instead of (T, E)).
            static_assert(Traits::IsUniq<
                std::decay_t<
                    typename Traits::CallableTraits<Vs>::template Argument<0>
                >...
            >, "Each type may be handled by one visitor only.");

            using argument_eligible_tags = Traits::TypeSequence<
                typename Traits::Filter<
                    Utils::IsTagForType,
                    Ts...
                >::template Execute<
                    std::decay_t<
                        typename Traits::CallableTraits<Vs>::template Argument<0>
                    >
                >...
            >; // This is a TypeSequence<TypeSequence<...>...> for each type of the union.
               // Each element of the outer sequence is a collection of tags that fit the
               // corresponding n-th visitor.

            _VisitImpl(argument_eligible_tags{}, std::forward<Vs&&>(fns)...);
        }

        /**
         * Returns true if the union is not in the uninitialized state.
         */
        bool IsInitialized() const noexcept { return _selection != static_cast<E>(0xCDCDCDCF); }

        /**
         * Returns the current enumeration of the union.
         */
        E GetSelection() const noexcept { return _selection; }

        /**
         * Returns a pointer to the underlying storage, as any type, regardless of the current state of the union.
         * You probably don't want to use this.
         */
        template <typename T> T* UnsafeAccess() noexcept { return std::launder(reinterpret_cast<T*>(data())); }

    private:
        template <typename... TagSequences, typename... Vs>
        void _VisitImpl(Traits::TypeSequence<TagSequences...>, Vs&&... visitors) {
            auto _ = (_VisitOneImpl(TagSequences{}, visitors) || ...);
        }

        template <typename V, typename... Tags>
        bool _VisitOneImpl(Traits::TypeSequence<Tags...>, V&& visitor) {
            static_assert(sizeof...(Tags) > 0, "One visitor processes a type the union does not store.");

            if (((Tags::key == _selection) || ...)) {
                Utils::DispatchInvoke(visitor, *UnsafeAccess<
                    Traits::Head<
                        Traits::TypeSequence<
                            typename Utils::TypeOfTag<Tags>::type...
                        >
                    >
                >(), _selection);
                return true;
            }

            return false;
        }

        const std::byte* data() const noexcept { return &_storage[0]; }
        std::byte* data() noexcept { return &_storage[0]; }

        E _selection = {};
        alignas(typename Utils::TypeOfTag<Ts>::type...) std::byte _storage[std::max({ sizeof(typename Utils::TypeOfTag<Ts>::type)... })] = { };
    };

#undef MAKE_LVALUE

    template <auto K, typename T>
    struct Of {
        constexpr static const auto key = K;
        using value_type = T;

        template <decltype(K) K2, typename T2, typename = std::enable_if_t<K != K2>>
        using Or = Union<decltype(K), Of<K, T>, Of<K2, T2>>;

        [[nodiscard]] static bool TryAllocate(decltype(K) selection, std::byte* storage) noexcept (noexcept(T{})) {
            if (selection != K)
                return false;

            T* instance = new (storage) T(); // placement-new
            return instance != nullptr;
        }

        [[nodiscard]] static bool TryFree(decltype(K) selection, std::byte* storage) noexcept(std::is_nothrow_destructible_v<T>) {
            if (selection != K)
                return false;

            std::launder(reinterpret_cast<T*>(storage))->~T(); // placement new forbids calling delete; call dtor manually
            return true;
        }
    };
}
