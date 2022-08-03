#pragma once
#include <iostream>
#include <string_view>
#include <utility>
#include <algorithm>
#include <concepts>
#include <cstddef>
#include <initializer_list>

namespace kaixo {
    /*
    Short explanation how it all works

    List comprehension has:
     - Result expression
     - Parts: containers, constraints, breaking conditions, etc.
     - Additional definitions from the outside

     What makes a part a part?
     There are 2 forward_types of parts: container, or executable. All container parts
     are a specialization of a linked_container. An executable simply defines an
     execute() method that takes a reference to a named_tuple.

     What is a linked_container?
     It's a wrapper for a tuple of other containers, which are then linked to
     variables. It will do parallel iteration on all its containers. It
     defines definitions and dependencies, depending on the linked variables
     and the dependencies of the nested containers.

     An executable can also optionally define dependencies and definitions, if
     it defines definitions it has to define a templated definition_types that
     will be given a named_tuple of at least all its dependencies, so it can
     evaluate the forward_types of its definitions.

     A container can also define dependencies, and if it does, it has to define
     a templated give_dependencies method, which will be given a named_tuple of
     at least all its dependencies. With this information it can deduce a more
     complete type if necessary, and return a new instance of the complete type.
     Making the necessary copies of its current data. On top of this, the returned
     type from that function should define a method give() with auto& argument.
     That function will be called with a named_tuple of at least all its dependencies
     right before 'begin' is called on the container, so it can take the new
     values of its dependencies into account when starting a new iteration.
    */

    // Return codes for executing certain list comprehension parts
    enum ReturnCode {
        NONE = 0,  // Continue like normal
        BREAK = 1, // Stop iteration now, and set to end
        AGAIN = 2  // Skip current values, and increment again
    };

    // Some handy dandy helpers, mainly for dealing with tuples
    inline namespace detail {

        // Some handy tuple operations
        template<class> struct tail;
        template<class Ty, class ...Tys>
        struct tail<std::tuple<Ty, Tys...>> { using type = std::tuple<Tys...>; };
        template<class Ty> using tail_t = typename tail<Ty>::type;
        template<class> struct head;
        template<class Ty, class ...Tys>
        struct head<std::tuple<Ty, Tys...>> { using type = Ty; };
        template<class Ty> using head_t = typename head<Ty>::type;

        // Remove duplicate forward_types from tuple
        template<std::size_t i, class Tuple, std::size_t... is>
        constexpr auto element_as_tuple(Tuple tuple, std::index_sequence<is...>) {
            if constexpr (!(std::is_same_v<std::tuple_element_t<i, Tuple>,
                std::tuple_element_t<is, Tuple>> || ...))
                return std::tuple<std::tuple_element_t<i, Tuple>>(std::get<i>(tuple));
            else return std::make_tuple();
        }
        template<class Tuple, std::size_t... is>
        constexpr auto make_tuple_unique(Tuple tuple, std::index_sequence<is...>) {
            return std::tuple_cat(element_as_tuple<is>(tuple, std::make_index_sequence<is>())...);
        }
        template<class... Tuples> constexpr auto make_tuple_unique(Tuples... tuples) {
            return make_tuple_unique(std::tuple_cat(tuples...),
                std::make_index_sequence<std::tuple_size_v<decltype(std::tuple_cat(tuples...))>>{});
        }
        template<class ...Tys> using unique_tuple_t = decltype(make_tuple_unique(std::declval<Tys>()...));

        // Specialization of templated class
        template<class, template<class...> class>
        struct is_specialization : std::false_type {};
        template<template<class...> class Ref, class... Args>
        struct is_specialization<Ref<Args...>, Ref> : std::true_type {};
        template<class Test, template<class...> class Ref>
        concept specialization = is_specialization<std::decay_t<Test>, Ref>::value;

        // Get index of type in tuple
        template<class T, class E, std::size_t I> struct tuple_index_r;
        template<class F, class ...R, class E, std::size_t I>
        struct tuple_index_r<std::tuple<F, R...>, E, I> : public std::conditional<std::is_same<E, F>::value,
            std::integral_constant<std::size_t, I>, tuple_index_r<std::tuple<R...>, E, I + 1>>::type{};
        template<class E, std::size_t I> struct tuple_index_r<std::tuple<>, E, I> {};
        template<class E, class Tuple>
        constexpr static std::size_t tuple_index = tuple_index_r<Tuple, E, 0>::value;

        // Check if type is in tuple
        template<class T, class Ty> struct in_tuple_d;
        template<class T, class ...Tys> struct in_tuple_d<T, std::tuple<Tys...>>
        : std::bool_constant<std::disjunction_v<std::is_same<T, Tys>...>> {};
        template<class T, class Ty> concept in_tuple = in_tuple_d<T, Ty>::value;
        template<class T, class Ty> concept not_in_tuple = !in_tuple_d<T, Ty>::value;

        // Concat tuples
        template<class ...Tys> using tuple_cat_t = decltype(std::tuple_cat(std::declval<Tys>()...));

        // Removes all forward_types in tuple from other tuple
        struct dud {}; // dud type
        template<class Ty, class T> struct remove_from;
        template<class Ty, class ...E> struct remove_from<Ty, std::tuple<E...>> {
            using type = tail_t< // Tail = remove first, so first (and now only) dud
                unique_tuple_t<std::tuple<dud, // Start = dud type -> unique_tuple -> remove other duds
                std::conditional_t<in_tuple<E, Ty>, dud, E>...>>>; // If in tuple: dud, otherwise type
        };
        template<class Ty, class V> using remove_from_t = typename remove_from<Ty, V>::type;

        template<class Ty> struct fake { using type = Ty; }; // Fake type

        // Converts pair or type to tuple, and keeps tuple.
        template<class Ty> struct as_tuple { using type = std::tuple<Ty>; };
        template<class A, class B> struct as_tuple<std::pair<A, B>> {
            using type = std::tuple<std::remove_const_t<A>, B>;
        };
        template<class ...Tys> struct as_tuple<std::tuple<Tys...>> {
            using type = std::tuple<Tys...>;
        };
        template<class Ty> using as_tuple_t = typename as_tuple<Ty>::type;

        // Flattens single nested tuples into their forward_types <int, tuple<int, int>> -> <int, int, int>
        template<class> struct flatten_tuple {};
        template<class ...Tys> struct flatten_tuple<std::tuple<Tys...>> {
            using type = tuple_cat_t<as_tuple_t<Tys>...>;
        };
        template<class Ty> using flatten_tuple_t = typename flatten_tuple<Ty>::type;
    }

    template<std::size_t N> struct tag_t {
        char value[N - 1];
        constexpr tag_t(const char(&val)[N]) : value() { std::copy_n(val, N - 1, value); }
    };

    template<tag_t Name> struct var_t {
        constexpr static auto name = Name;
        using definitions = std::tuple<var_t<Name>>; // Variable defines and depends on itself
        using dependencies = std::tuple<var_t<Name>>; // Cancels out, but helpful for generalizing
    };
    template<tag_t Name> constexpr auto var = var_t<Name>{};

    // Concepts
    template<class Ty> concept var_type = requires() { std::decay_t<Ty>::name; };
    template<class Ty> concept collection = specialization<Ty, std::tuple>;
    template<class Ty> concept has_dependencies = requires () { typename Ty::dependencies; };
    template<class Ty> concept has_definitions = requires () { typename Ty::definitions; };
    template<class Ty> concept container = requires(std::decay_t<Ty> ty) { ty.begin(); ty.end(); };
    template<class Ty, class Arg>
    concept has_give_dependencies = requires (Ty ty) { ty.template give_dependencies<Arg>(); };
    template<class Ty, class Arg>
    concept has_definition_types = requires () { typename Ty::template definition_types<Arg>; };

    // Get iterator type by checking return type of begin on const ref of type
    template<class Ty> using iterator_type_t = decltype(std::declval<std::decay_t<Ty>&>().begin());

    // Retrieve certain aspects of list comprehension parts
    template<class> struct get_dependencies {
        using type = std::tuple<>;
    }; // ^ If doesn't have any defined dependencies, just empty tuple

    template<has_dependencies Ty> struct get_dependencies<Ty> {
        using type = typename Ty::dependencies;
    }; // ^ If defined dependencies, get those

    template<class ...Tys> struct get_dependencies<std::tuple<Tys...>> {
        using type = tuple_cat_t<typename get_dependencies<std::decay_t<Tys>>::type...>;
    }; // ^ Tuple of forward_types: recurse to get all dependencies, and then tuple cat them

    template<class Ty>
    using get_dependencies_t = typename get_dependencies<std::decay_t<Ty>>::type;

    template<class>
    struct get_definitions {
        using type = std::tuple<>;
    }; // ^ If doesn't have any defined definitions, just empty tuple

    template<has_definitions Ty>
    struct get_definitions<Ty> {
        using type = typename Ty::definitions;
    }; // ^ If defined definitions, get those

    template<class ...Tys>
    struct get_definitions<std::tuple<Tys...>> {
        using type = tuple_cat_t<typename get_definitions<std::decay_t<Tys>>::type...>;
    }; // ^ Tuple of forward_types: recurse to get all definitions, and then tuple cat them

    template<class Ty>
    using get_definitions_t = typename get_definitions<std::decay_t<Ty>>::type;

    template<class> struct get_iterator_data {
        using type = char;
    }; // If no container, just use small type as iterator data (easier for generalizing)

    template<container Ty> struct get_iterator_data<Ty> {
        using type = iterator_type_t<Ty>;
    }; // If container, get iterator

    template<class ...Tys> struct get_iterator_data<std::tuple<Tys...>> {
        using type = std::tuple<typename get_iterator_data<std::decay_t<Tys>>::type...>;
    }; // Tuple recurse 

    // Get iterator data, if not container: char (makes it easier than exclusing non-containers)
    template<class Ty> using get_iterator_data_t =
        typename get_iterator_data<std::decay_t<Ty>>::type;

    template<class, class> struct get_definition_types {
        using type = std::tuple<>;
    }; // No definitions: empty tuple

    template<class Arg, container Ty> requires (!has_definition_types<Ty, Arg>)
        struct get_definition_types<Arg, Ty> {
        using type = std::tuple<typename std::decay_t<Ty>::value_type>;
    }; // Container defines its value forward_types 

    template<class Arg, has_definition_types<Arg> Ty> struct get_definition_types<Arg, Ty> {
        using type = typename std::decay_t<Ty>::template definition_types<Arg>;
    }; // If definitions, get them

    template<class Arg, class ...Tys> struct get_definition_types<Arg, std::tuple<Tys...>> {
        using type = tuple_cat_t<typename get_definition_types<Arg, std::decay_t<Tys>>::type...>;
    }; // Tuple recurse

    // Get the forward_types of the definitions, given a named_tuple
    template<class Arg, class Ty> using get_definition_types_t =
        typename get_definition_types<Arg, std::decay_t<Ty>>::type;

    // Give dependencies gives a named_tuple of at least all the callee's dependencies so
    // it can infer type information using these dependencies. If it doesn't define this
    // function, it will simply return itself (copy, as it'll be stored like this).
    template<class Add, has_give_dependencies<Add> Ty>
    constexpr inline decltype(auto) give_dependencies(Ty& val) { return val.template give_dependencies<Add>(); }
    template<class Add, class Ty> requires(!has_give_dependencies<Ty, Add>)
        constexpr inline Ty give_dependencies(Ty& val) { return val; }

    // Var tuple, lists all nested definitions and dependencies
    template<var_type ...As> struct var_tuple_t {
        using definitions = tuple_cat_t<typename As::definitions...>;
        using dependencies = tuple_cat_t<typename As::dependencies...>;
    };
    template<class Ty> concept var_tuple = specialization<Ty, var_tuple_t>;

    // Named tuple, connects forward_types to names
    template<collection Vars = std::tuple<>, collection Types = std::tuple<>>
    struct named_tuple : public Types {
        static_assert(std::tuple_size_v<Vars> == std::tuple_size_v<Types>, "Tuple sizes do not match");
        using definition_types = Types;
        using definitions = Vars;

        template<var_type Var> using definition_type = std::tuple_element_t<tuple_index<Var, Vars>, Types>;

        template<class T, var_type ...V>
        constexpr inline void assign(const named_tuple<std::tuple<V...>, T>& vals) {
            (set<V>(vals.get<V>()), ...);
        }

        template<in_tuple<Vars> Var>
        constexpr inline decltype(auto) get() const {
            return std::get<tuple_index<Var, Vars>>(*this);
        }

        template<in_tuple<Vars> Var, class Ty>
        constexpr inline void set(Ty&& val) {
            std::get<tuple_index<Var, Vars>>(*this) = std::forward<Ty>(val);
        }

        template<not_in_tuple<Vars> Var, class Ty>
        constexpr inline void set(Ty&&) {} // Empty overload, in case var isn't in here.
    };

    // Expression type, practically wrapper for lambda, but keeps track of dependencies
    template<class Lambda, collection Vars> struct expression_t : Lambda, fake<Vars> {
        using dependencies = unique_tuple_t<Vars>;

        // This execute is for list comprehension constraints, as an expression as 
        // a standalone part in a comprehension object acts as one.
        constexpr inline int execute(auto& vals, auto&) const {
            return Lambda::operator()(vals) // If expression passes
                ? ReturnCode::NONE   // Continue iteration like normal
                : ReturnCode::AGAIN; // otherwise, skip current values and iterate again
        }
    };

    template<class Ty> concept expression = specialization<Ty, expression_t>;

    // Breaking conditions, wrapper for an expression, when expression 
    // evaluates to true, it will stop iteration.
    template<expression Definition> struct break_condition_t : Definition {
        using dependencies = get_dependencies_t<Definition>;

        constexpr inline int execute(auto& vals, auto&) const {
            return Definition::operator()(vals) // If breaking condition passes
                ? ReturnCode::BREAK // Break iteration
                : ReturnCode::NONE; // otherwise continue iteration like normal
        }
    };

    template<class Ty> concept break_condition = specialization<Ty, break_condition_t>;

    // Var definition, wrapper for expression. Assigns result of expression to the var
    template<expression Definition, var_type Var> struct var_definition : Definition, Var {
        using dependencies = get_dependencies_t<Definition>;
        using definitions = std::tuple<Var>;
        template<class Arg> using definition_types = // Type is result of expression
            std::tuple<decltype(std::declval<Definition>()(std::declval<Arg&>()))>;

        constexpr inline int execute(auto& vals, auto&) const {
            vals.set<Var>(Definition::operator()(vals));
            return ReturnCode::NONE; // Resume normal iteration
        }
    };

    template<expression Definition, var_tuple Vars> struct var_unpack : Definition, Vars {
        using dependencies = get_dependencies_t<Definition>;
        using definitions = get_definitions_t<Vars>;
        template<class Arg> using definition_types =
            as_tuple_t<decltype(std::declval<Definition>()(std::declval<Arg&>()))>;

        constexpr inline int execute(auto& vals, auto&) const {
            using named_tuple_type = named_tuple<definitions,
                definition_types<std::decay_t<decltype(vals)>>>;
            vals.assign(named_tuple_type{ Definition::operator()(vals) });
            return ReturnCode::NONE;
        }
    };

    // Links a collection of containers to a collection of vars, either to 1 var (tuple) or
    // exactly the same amount of vars (one-to-one). Does parallel iteration of all containers
    template<collection Containers, collection Vars = std::tuple<>, class Additional = named_tuple<>>
    struct linked_container_t : Containers {
        using containers = Containers;
        using size_type = std::size_t;
        using definitions = Vars;
        using dependencies = get_dependencies_t<containers>;

        template<bool, class Arg> struct concat_types {
            using type = flatten_tuple_t<get_definition_types_t<Arg, Containers>>;
        };
        // Special case for when have dependencies, because then we don't have all type
        // information, and trying to get definition forward_types would give compile error.
        template<class Arg> struct concat_types<false, Arg> {
            using type = std::tuple<>;
        };

        template<class Arg> using concat_types_t =
            typename concat_types<std::tuple_size_v<dependencies> == 0, Arg>::type;

        constexpr static bool many_to_one = std::tuple_size_v<Vars> <= 1;
        constexpr static bool one_to_one = std::tuple_size_v<Containers> == std::tuple_size_v<Vars>;
        constexpr static bool flattened_one_to_one = std::tuple_size_v<dependencies> != 0 ||
            std::tuple_size_v<concat_types_t<Additional>> == std::tuple_size_v<Vars>;
        static_assert(many_to_one || one_to_one || flattened_one_to_one,
            "Amount of linked variables is not compatible with the amount of containers.");

        template<class Arg> using definition_types =
            // #containers == #vars: normal, one-to-one, container->var
            std::conditional_t<one_to_one, get_definition_types_t<Arg, containers>,
            // #forward_types == #vars: tuple cat, flattened one-to-one, forward_types->var
            std::conditional_t<flattened_one_to_one, flatten_tuple_t<get_definition_types_t<Arg, containers>>,
            // #vars == 1: wrap in tuple, many-to-one, var becomes tuple
            std::tuple<get_definition_types_t<Arg, containers>>>>;
        // Cannot define a value_type, as there might be undefined dependencies at one point

        class iterator {
        public:
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using size_type = linked_container_t::size_type;

            using iterator_data = get_iterator_data_t<containers>;
            iterator_data data;

            constexpr iterator() {}
            constexpr iterator(containers& me, bool e) { e ? end<0>(me) : begin<0>(me); }
            constexpr iterator(const containers& me, bool e) { e ? end<0>(me) : begin<0>(me); }

            constexpr iterator(iterator&&) = default;
            constexpr iterator(const iterator&) = default;
            constexpr iterator& operator=(iterator&&) = default;
            constexpr iterator& operator=(const iterator&) = default;

            constexpr iterator& operator++() { increment<0>(); return *this; }

            constexpr bool operator==(const iterator& other) const { return equal<0>(other); }
            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }

            constexpr decltype(auto) operator*() {
                return value(std::make_index_sequence<std::tuple_size_v<containers>>{});
            }

        private:
            template<std::size_t I> constexpr inline void increment() {
                ++std::get<I>(data);
                if constexpr (I != std::tuple_size_v<containers> -1) increment<I + 1>();
            }

            template<std::size_t I> constexpr inline void begin(auto& me) {
                std::get<I>(data) = std::get<I>(me).begin();
                if constexpr (I != std::tuple_size_v<containers> -1) begin<I + 1>(me);
            }

            template<std::size_t I> constexpr inline void end(auto& me) {
                std::get<I>(data) = std::get<I>(me).end();
                if constexpr (I != std::tuple_size_v<containers> -1) end<I + 1>(me);
            }

            template<std::size_t I> constexpr inline bool equal(const iterator& other) const {
                if constexpr (I == std::tuple_size_v<containers>) return false;
                else return std::get<I>(data) == std::get<I>(other.data) || equal<I + 1>(other);
            }

            template<std::size_t ...Is> constexpr inline auto value(std::index_sequence<Is...>) {
                if constexpr (sizeof...(Is) == 1) return *std::get<Is...>(data);
                else { // If flattened, tuple cat results
                    if constexpr (flattened_one_to_one)
                        return std::tuple_cat(std::tuple{ *std::get<Is>(data) }...);
                    else return std::tuple{ *std::get<Is>(data)... };
                }
            }
        };

        using const_iterator = iterator;

        constexpr inline iterator begin() { return iterator{ *this, false }; }
        constexpr inline iterator end() { return iterator{ *this, true }; }
        constexpr inline iterator begin() const { return iterator{ *this, false }; }
        constexpr inline iterator end() const { return iterator{ *this, true }; }

        template<class Add> constexpr inline auto give_dependencies() const {
            return give_dependencies_i<Add>(std::make_index_sequence<std::tuple_size_v<Containers>>{});
        }

        template<class Add, std::size_t ...Is>
        constexpr inline auto give_dependencies_i(std::index_sequence<Is...>) const {

            // Get the type of the new tuple of containers after giving their dependencies
            using with_added = decltype(std::tuple{
                kaixo::give_dependencies<Add>(std::get<Is>(*(Containers*)this))... });

            // Give all containers their dependencies, return new linked container with this type info
            return linked_container_t<with_added, Vars, Add> {
                std::tuple{ kaixo::give_dependencies<Add>(std::get<Is>(*(Containers*)this))... }
            };
        }

        constexpr inline void give(auto& vals) const {
            static_assert(std::tuple_size_v<dependencies> == 0,
                "Can only evaluate as const if containers have no dependencies");
        }
        constexpr inline void give(auto& vals) { give<0>(vals); }

        template<std::size_t I> constexpr inline auto give(auto& vals) {
            using type = std::tuple_element_t<I, containers>;
            // If a container has dependencies, it will be given the current values
            if constexpr (has_dependencies<type>) std::get<I>(*this).give(vals);
            if constexpr (I != std::tuple_size_v<containers> -1) give<I + 1>(vals);
        }
    };

    template<class Ty> concept linked_container = specialization<std::decay_t<Ty>, linked_container_t>;

    // Specialization of as_tuple for linked_container, define as its containers
    template<linked_container Ty> struct as_tuple<Ty> {
        using type = typename std::decay_t<Ty>::containers;
    };

    // Recursively deduce forward_types of all definitions by giving it the previous named_tuple
    template<class Parts, class Add, std::size_t I> struct named_tuple_type_i {
        using prev_type = typename named_tuple_type_i<Parts, Add, I - 1>::type;
        using part_type = std::tuple_element_t<I - 1, Parts>;
        using type = named_tuple<
            tuple_cat_t<typename prev_type::definitions, get_definitions_t<part_type>>,
            tuple_cat_t<typename prev_type::definition_types, get_definition_types_t<prev_type, part_type>>>;
    };

    template<class Parts, class Add> struct named_tuple_type_i<Parts, Add, 0> { using type = Add; };
    template<class Parts, class Add = named_tuple<>> using named_tuple_type_c =
        typename named_tuple_type_i<Parts, Add, std::tuple_size_v<Parts>>::type;

    // List comprehension construct is an intermediate instance, used while still
    // constructing the final comprehension object.
    template<expression Result, collection Parts, class Additional = named_tuple<>>
    struct list_comprehension_construct {
        Result result; // Expression that determines output type
        Parts parts;   // All parts of the comprehension
        Additional additional; // Additional variables passed by an outside source

        // Get all dependencies and definitions of the parts, and deduce any leftover dependencies 
        using prior_dependencies = tuple_cat_t<get_dependencies_t<Parts>, get_dependencies_t<Result>>;
        using definitions = tuple_cat_t<get_definitions_t<Parts>, get_definitions_t<Additional>>;
        using dependencies = remove_from_t<definitions, prior_dependencies>;
    };

    template<class A, class B>
    list_comprehension_construct(A&&, B&&)->list_comprehension_construct<A, B>;

    template<class Ty> concept lc_construct = specialization<Ty, list_comprehension_construct>;

    // Final list comprehension object, has no more dependencies, as they should
    // all be resolved when creating an instance of this class.
    template<expression Result, collection Parts, class Additional>
    struct list_comprehension : list_comprehension_construct<Result, Parts, Additional> {
        // Type of the named_tuple that will be given to the result expression
        // contains all dependencies of all the parts.
        using named_tuple_type = named_tuple_type_c<Parts, Additional>;

        using size_type = std::size_t;
        using value_type = decltype(std::declval<Result>()(std::declval<named_tuple_type&>()));

        template<bool Const> class iterator_t {
            using iterator_data = get_iterator_data_t<Parts>;
            using me_type = std::conditional_t<Const, const list_comprehension*, list_comprehension*>;
        public:
            using iterator_category = std::input_iterator_tag;
            using value_type = list_comprehension::value_type;
            using reference = value_type;
            using difference_type = std::ptrdiff_t;
            using size_type = list_comprehension::size_type;

            constexpr iterator_t() {}
            constexpr iterator_t(me_type me, bool end) : me(me), end(end) { if (!end) prepare(); }

            constexpr iterator_t(iterator_t&&) = default;
            constexpr iterator_t(const iterator_t&) = default;
            constexpr iterator_t& operator=(iterator_t&&) = default;
            constexpr iterator_t& operator=(const iterator_t&) = default;

            constexpr iterator_t& operator++() {
                increment<std::tuple_size_v<Parts> -1>();
                return *this;
            }

            constexpr bool operator!=(const iterator_t& other) const { return !operator==(other); }
            constexpr bool operator==(const iterator_t& other) const {
                return end == true && other.end == true // If end, iterators don't matter.
                    || other.end == end && other.data == data;
            }

            constexpr value_type operator*() {
                // Throw at compiletime if access past end
                if (std::is_constant_evaluated() && end) throw;
                return me->result(values);
            }

        private:
            bool end = true;
            me_type me = nullptr;
            named_tuple_type values{};
            iterator_data data{};

            // How does this increment work?
            // E: Executable, C: Container
            // lc - C E C C E E C E E
            //                  |<<<< first increment 
            //            |<<<<<|     if == end: increment next one
            //          |<|           if == end: increment next one
            //          |             if != end: done
            //          |>|           execute till previous container
            //            |           set to begin
            //            |>>>>>|     execute till previous container   
            //                  |     set to begin
            //                  |>>>> execute till start
            // This way, only executables that rely on changed values will be executed.
            template<std::size_t I> constexpr inline void increment() {
                using type = std::tuple_element_t<I, Parts>;
                if constexpr (linked_container<type>) { // Only increment if container
                    auto& _part = std::get<I>(me->parts);
                    auto& _data = std::get<I>(data);
                    do {
                        if (++_data == _part.end()) { // Increment and check for end
                            if constexpr (I != 0) increment<I - 1>(); // recurse down to next container
                            _part.give(values); // Give values to the part
                            _data = _part.begin(); // Reset to begin
                            // If I == 0: reached end of final container, so end = true
                            if constexpr (I == 0) { end = true; return; }
                        }
                        // Assign the current value to the values tuple
                        values.assign(named_tuple<get_definitions_t<type>,
                            get_definition_types_t<named_tuple_type, type>>{ *_data });
                        if constexpr (I != std::tuple_size_v<Parts> -1) {
                            auto _code = execute<I + 1>(); // Execute executables
                            // If break, we're done, if again, increment again, otherwise nothing
                            if (end || _code & ReturnCode::BREAK) { end = true; return; }
                            else if (_code & ReturnCode::AGAIN) continue;
                        }
                        return;
                    } while (true);
                }
                else if constexpr (I == 0) { end = true; return; }// We're at the end!
                else return increment<I - 1>();
            }

            // Will execute recursively until it hits a container, or the end.
            template<std::size_t I> constexpr inline int execute() {
                using type = std::tuple_element_t<I, Parts>;
                if constexpr (!linked_container<type>) { // containers don't execute anything
                    int _code = 0;
                    auto& _part = std::get<I>(me->parts);
                    auto& _data = std::get<I>(data);
                    _code |= _part.execute(values, _data);
                    if constexpr (I != std::tuple_size_v<Parts> -1)
                        return _code | execute<I + 1>(); // Recurse to next element
                    else return _code;
                }
                else return ReturnCode::NONE;
            }

            // Recursively give initial values
            template<std::size_t I> constexpr inline int begin() {
                using type = std::tuple_element_t<I, Parts>;
                int _code = 0;
                auto& _part = std::get<I>(me->parts);
                auto& _data = std::get<I>(data);
                if constexpr (linked_container<type>) {
                    _part.give(values); // Give current values
                    _data = _part.begin(); // Set iterator to begin, and assign to values tuple
                    values.assign(named_tuple<get_definitions_t<type>,
                        get_definition_types_t<named_tuple_type, type>>{ *_data });
                }
                else _code |= _part.execute(values, _data);
                if constexpr (I != std::tuple_size_v<Parts> -1) return _code | begin<I + 1>();
                else return _code;
            }

            constexpr inline void prepare() {
                values.assign(me->additional);
                int _code = begin<0>(); // Set iterators to begin
                if (!(_code & ReturnCode::BREAK) && (_code & ReturnCode::AGAIN))
                    operator++(); // Set to valid state if currently not.
            }
        };

        using iterator = iterator_t<false>;
        using const_iterator = iterator_t<true>;

        constexpr inline iterator begin() { return iterator{ this, false }; }
        constexpr inline iterator end() { return iterator{ this, true }; }
        constexpr inline const_iterator begin() const { return const_iterator{ this, false }; }
        constexpr inline const_iterator end() const { return const_iterator{ this, true }; }

        // Add index operator only for constexpr, as it's not optimal
        consteval decltype(auto) operator[](size_type index) const {
            auto _it = begin();
            while (index--) ++_it;
            return *_it;
        }

        consteval size_type size() const {
            auto _it = begin();
            size_type _index = 0;
            while (_it != end()) ++_it, ++_index;
            return _index;
        }

        constexpr inline void give(auto& vals) { this->additional.assign(vals); }

        template<container Ty> requires (!specialization<Ty, std::initializer_list>)
            constexpr operator Ty() { return Ty{ begin(), end() }; }
    };

    template<class A, class B>
    list_comprehension(list_comprehension_construct<A, B>&&)->list_comprehension<A, B, named_tuple<>>;

    // Incomplete list comprehension is created when there's still dependencies
    template<expression Result, collection Parts>
    struct incomplete_list_comprehension : list_comprehension_construct<Result, Parts> {
        template<class Add> using definition_types = std::tuple<decltype(
            std::declval<Result>()(std::declval<named_tuple_type_c<Parts, std::decay_t<Add>>&>()))>;

        // Define the give_dependencies, if complete now, it will return the final
        // and complete list comprehension object, otherwise another incomplete instance.
        template<class Add> constexpr inline auto give_dependencies() const {
            using named_tuple_type = named_tuple_type_c<Parts, Add>; // Combine parts with added
            // Recurse down, and give all the parts these new dependencies as well.
            auto _newparts = give_dependencies_i<named_tuple_type>(
                std::make_index_sequence<std::tuple_size_v<Parts>>{});
            // Return the selected type, with the new parts
            return list_comprehension<Result, decltype(_newparts), named_tuple_type> {
                list_comprehension_construct<Result, decltype(_newparts), named_tuple_type>{
                    .result = this->result, .parts = std::move(_newparts),
                } };
        };

        template<class Add, std::size_t ...Is>
        constexpr inline auto give_dependencies_i(std::index_sequence<Is...>) const {
            return std::tuple{ kaixo::give_dependencies<Add>(std::get<Is>(this->parts))... };
        }

        constexpr int begin() const { return 0; } // Necessary to make it a 'container'
        constexpr int end() const { return 0; }
    };

    template<var_type Var, container Container>
    struct var_container : Container {
        using dependencies = std::tuple<Var>;
        using value_type = typename Container::value_type;

        constexpr inline void give(auto& vals) {
            Container::operator=(vals.get<Var>());
        }

        using Container::begin;
        using Container::end;
    };

    template<var_type Var>
    struct incomplete_var_container {
        using dependencies = std::tuple<Var>;
        template<class Add> using definition_types =
            get_definition_types_t<Add, typename Add::template definition_type<Var>>;

        template<class Add> constexpr inline auto give_dependencies() const {
            return var_container<Var, typename Add::template definition_type<Var>>{};
        }

        constexpr int begin() const { return 0; } // Necessary to make it a 'container'
        constexpr int end() const { return 0; }
    };

    // Operators for constructing a list comprehension object
    inline namespace lc_operators {

        // Overloads for using several type groups in expressions
        template<class T, var_type ...Ty>
        constexpr inline decltype(auto) use(T& vals, const var_tuple_t<Ty...>&) {
            return std::tuple{ vals.get<std::decay_t<Ty>>()... };
        }

        template<class T, var_type Ty>
        constexpr inline decltype(auto) use(T& vals, Ty&&) {
            return vals.get<std::decay_t<Ty>>();
        }

        template<class T, expression Ty>
        constexpr inline decltype(auto) use(T& vals, Ty&& expr) {
            return expr(vals);
        }

        template<class T, container Ty>
        constexpr inline decltype(auto) use(T& vals, Ty&& cont) {
            if constexpr (has_dependencies<std::decay_t<Ty>>) {
                auto res = cont.template give_dependencies<std::decay_t<T>>();
                if constexpr (std::tuple_size_v<get_dependencies_t<decltype(res)>> != 0)
                    return std::forward<Ty>(cont);
                else {
                    res.give(vals);
                    return res;
                }
            }
            else return std::forward<Ty>(cont);
        }

        template<class T, class Ty>
        constexpr inline decltype(auto) use(T& vals, Ty&& val) {
            return std::forward<Ty>(val);
        }

        template<class Ty> concept valid_op_arg = !container<Ty> && !specialization<Ty, var_definition>
        && !specialization<Ty, break_condition_t> && !specialization<Ty, var_unpack>;
        template<class Ty> concept either_op_arg = var_type<std::decay_t<Ty>> || expression<Ty>;

#define create_op(op)                                                                                      \
        template<valid_op_arg A, valid_op_arg B> constexpr inline decltype(auto) operator op(A&& a, B&& b) \
            requires (either_op_arg<A> || either_op_arg<B>){                                               \
            return expression_t{ [a = std::forward<A>(a), b = std::forward<B>(b)]<class Ty>(Ty& vals) {    \
                return use(vals, a) op use(vals, b); },                                                    \
                fake<tuple_cat_t<get_dependencies_t<A>, get_dependencies_t<B>>>{} };                       \
        }

        create_op(+) create_op(-) create_op(*) create_op(/ ) create_op(== ) create_op(!= );
        create_op(<= ) create_op(>= ) create_op(> ) create_op(< ) create_op(%) create_op(<=> );
        create_op(<< ) create_op(>> ) create_op(&) create_op(| ) create_op(&&) create_op(|| );

#define create_uop(op)                                                                   \
        template<either_op_arg A> constexpr inline decltype(auto) operator op(A&& a) {   \
            return expression_t{ [a = std::forward<A>(a)]<class Ty>(Ty& vals) {          \
                return op use(vals, a); }, fake<tuple_cat_t<get_dependencies_t<A>>>{} }; \
        }

        create_uop(~) create_uop(!) create_uop(*) create_uop(&);

        template<class Ty> concept valid_result_arg =
            var_type<std::decay_t<Ty>> || var_tuple<Ty> || expression<Ty>;

        // Construct tuple of variables
        template<var_type A, var_type B>
        constexpr inline var_tuple_t<A, B> operator,(A&, B&) { return {}; }
        template<var_type A, var_type ...B>
        constexpr inline var_tuple_t<A, B...> operator,(A&, var_tuple_t<B...>&&) { return {}; }
        template<var_type ...A, var_type B>
        constexpr inline var_tuple_t<A..., B> operator,(var_tuple_t<A...>&&, B&) { return {}; }
        template<var_type ...A, var_type ...B>
        constexpr inline var_tuple_t<A..., B...> operator,(var_tuple_t<A...>&&, var_tuple_t<B...>&&) { return {}; }

        // Construct tuple of expressions/variables
        template<valid_result_arg A, valid_result_arg B> constexpr inline auto operator,(A&& a, B&& b) {
            return expression_t{ [a = std::forward<A>(a), b = std::forward<B>(b)] <class Ty>(Ty & vals) {
                return std::tuple_cat(std::tuple{ use(vals, a) }, std::tuple{ use(vals, b) });
 },
fake<tuple_cat_t<get_dependencies_t<A>, get_dependencies_t<B>>>{} };
        }

        // Chain containers together for parallel iteration
        template<container A, container B> constexpr inline auto operator,(A&& a, B&& b) {
            return linked_container_t<tuple_cat_t<as_tuple_t<A>, as_tuple_t<B>>, std::tuple<>>{
                std::tuple_cat(as_tuple_t<A>{ std::forward<A>(a) }, as_tuple_t<B>{ std::forward<B>(b) }) };
        }

        // Convert single container into linked container, or forward linked container
        template<container A> constexpr inline auto operator-(A&& a) {
            if constexpr (linked_container<A>) return std::forward<A>(a);
            else return linked_container_t<std::tuple<A>, std::tuple<>>{ std::forward<A>(a) };
        }

        template<var_type A> constexpr inline auto operator-(A&) {
            return linked_container_t<std::tuple<incomplete_var_container<std::decay_t<A>>>, std::tuple<>>{};
        }

        // Link variables to linked container
        template<var_type A, linked_container B> constexpr inline auto operator<(A&, B&& b) {
            return linked_container_t<typename B::containers, get_definitions_t<A>>{ std::forward<B>(b) };
        }

        template<var_tuple A, linked_container B> constexpr inline auto operator<(A&&, B&& b) {
            return linked_container_t<typename B::containers, get_definitions_t<A>>{ std::forward<B>(b) };
        }

        // Start of list comprehension, combine result expression with a part
        template<class Ty> concept valid_starter = valid_result_arg<Ty> || (container<Ty> && has_dependencies<Ty>);
        template<valid_starter A, class Part> constexpr inline auto operator|(A&& a, Part&& part) {
            return list_comprehension_construct{
                expression_t{ [a = std::forward<A>(a)] <class Ty>(Ty & vals) {
                return use(vals, a);
 }, fake<get_dependencies_t<A>>{} },
std::tuple<Part>{ std::forward<Part>(part) } };
        }

        // Add parts to the list comprehension construct
        template<lc_construct Lc, class Part> constexpr inline auto operator,(Lc&& lc, Part&& part) {
            return list_comprehension_construct{
                std::move(lc.result),
                std::tuple_cat(std::move(lc.parts), std::tuple{ std::forward<Part>(part) })
            };
        }

        template<var_type A, expression B> constexpr inline auto operator<<=(A&, B&& b) {
            return var_definition{ std::forward<B>(b), A{} };
        }

        template<var_type A, var_tuple B> constexpr inline auto operator<<=(A&, B&& b) {
            return var_definition{
                expression_t{ [b = std::forward<B>(b)] (auto& vals) { return use(vals, b); },
                fake<get_dependencies_t<B>>{} }, A{} };
        }

        template<var_tuple A, expression B> constexpr inline auto operator<<=(A&&, B&& b) {
            return var_unpack{ std::forward<B>(b), A{} };
        }

        template<var_tuple A, var_type B> constexpr inline auto operator<<=(A&&, B&) {
            return var_unpack{
                expression_t{ [](auto& vals) { return vals.get<std::decay_t<B>>(); },
                fake<get_dependencies_t<B>>{} }, A{} };
        }

        struct brk_t {}; constexpr brk_t brk;
        template<expression B> constexpr inline auto operator<<=(const brk_t&, B&& b) {
            return break_condition_t{ std::forward<B>(b) };
        }
    }

    // List comprehension [] operator
    struct lc_op {
        template<lc_construct Lc> constexpr inline auto operator[](Lc&& lc) const {
            // If the list comprehension still has dependencies, forward it as a construct
            if constexpr (std::tuple_size_v<typename Lc::dependencies> != 0)
                return incomplete_list_comprehension{ std::forward<Lc>(lc) };
            else { // Otherwise construct the full object
                using named_tuple_type = named_tuple_type_c<decltype(lc.parts)>;

                // If complete, give the final dependencies to everything
                return list_comprehension{ list_comprehension_construct {
                    std::move(lc.result),
                    give_dependencies_i<named_tuple_type>(lc.parts,
                        std::make_index_sequence<std::tuple_size_v<decltype(lc.parts)>>{})
                } };
            }
        }

        template<class Add, std::size_t ...Is>
        constexpr inline auto give_dependencies_i(auto& parts, std::index_sequence<Is...>) const {
            return std::tuple{ kaixo::give_dependencies<Add>(std::get<Is>(parts))... };
        }
    };
    constexpr lc_op lc;

    // Simple range class, 
    struct inf_t {};
    constexpr inf_t inf;
    template<class Ty> concept var_or_dud = var_type<Ty> || std::same_as<Ty, dud>;
    template<class Ty> concept not_a_var = !var_type<Ty>;
    template<not_a_var Ty = int, var_or_dud Var1 = dud, var_or_dud Var2 = dud>
    class range {
    public:
        using dependencies =
            std::conditional_t<var_type<Var1>&& var_type<Var1>, std::tuple<Var1, Var2>,
            std::conditional_t<var_type<Var1>, std::tuple<Var1>,
            std::conditional_t<var_type<Var2>, std::tuple<Var2>, std::tuple<>>>>;

        template<class Add> constexpr inline auto give_dependencies() {
            if constexpr (var_type<Var1> && var_type<Var2>)
                return range<typename Add::template definition_type<Var1>, Var1, Var2>{};
            else if constexpr (var_type<Var1> || var_type<Var2>)
                return range<Ty, Var1, Var2>{ m_Start, m_End };
            else return range<Ty>{ m_Start, m_End };
        };

        constexpr inline void give(auto& vals) {
            if constexpr (var_type<Var1>) m_Start = vals.get<Var1>();
            if constexpr (var_type<Var2>) m_End = vals.get<Var2>();
        }

        using value_type = Ty;
        using size_type = std::size_t;

        constexpr range()
            : m_Start(), m_End() {}
        constexpr range(const Ty& a)
            : m_Start(), m_End(a) {}
        constexpr range(const Ty& a, const Ty& b)
            : m_Start(a), m_End(b < a ? a : b) {}
        constexpr range(const Ty& a, const Var2&)
            : m_Start(a), m_End(std::numeric_limits<Ty>::max() - 1) {}
        constexpr range(const Var1& a, const Var2&)
            : m_Start(std::numeric_limits<Ty>::min()), m_End(std::numeric_limits<Ty>::max() - 1) {}
        constexpr range(const Var1& a, const Ty& b)
            : m_Start(std::numeric_limits<Ty>::min()), m_End(b) {}
        constexpr range(const Ty& a, inf_t)
            : m_Start(a), m_End(std::numeric_limits<Ty>::max() - 1) {}

        class iterator {
        public:
            using value_type = range::value_type;
            using size_type = range::size_type;

            constexpr iterator() : val() {}
            constexpr iterator(iterator&& val) : val(val.val) {}
            constexpr iterator(const iterator& val) : val(val.val) {}
            constexpr iterator(Ty val) : val(val) {}
            constexpr iterator& operator=(iterator& other) { val = other.val; return *this; }
            constexpr iterator& operator=(const iterator& other) { val = other.val; return *this; }
            constexpr iterator& operator++() { ++val; return *this; }
            constexpr iterator& operator--() { --val; return *this; }
            constexpr bool operator==(const iterator& other) const { return other.val == val; }
            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr Ty operator*() const { return val; }

        private:
            Ty val;
        };

        using const_iterator = iterator;

        constexpr iterator begin() const { return iterator{ m_Start }; }
        constexpr iterator end() const { return iterator{ m_End + 1 }; }
        constexpr size_type size() const { return m_End - m_Start; }

        constexpr Ty operator[](size_type index) const { return m_Start + static_cast<Ty>(index); }

    private:
        Ty m_Start;
        Ty m_End;

        friend class iterator;
    };

    namespace lc_functions {
        template<class Ty> concept either_fun_arg = either_op_arg<Ty> || has_dependencies<Ty>;
        template<class Ty> concept valid_fun_arg = valid_op_arg<Ty> || has_dependencies<Ty>;
    }
}

// Define expression overloads for std functions 
#define lc_std_fun(y, x)                                                                          \
    template<kaixo::lc_functions::valid_fun_arg ...Args>                                          \
    requires(kaixo::lc_functions::either_fun_arg<Args> || ...) constexpr auto x(Args&&... args) { \
        return kaixo::expression_t{ [...args = std::forward<Args>(args)] (auto& vals) {           \
            return :: y x(kaixo::lc_operators::use(vals, args)...);                               \
        }, kaixo::fake<kaixo::tuple_cat_t<kaixo::get_dependencies_t<Args>...>> {} };              \
    }

#ifndef KAIXO_LC_FUNCTIONAL
#define KAIXO_LC_FUNCTIONAL 1
#endif
#if KAIXO_LC_FUNCTIONAL == 1
#include <functional>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, bind_front);
        lc_std_fun(std::, bind);
        lc_std_fun(std::, ref);
        lc_std_fun(std::, cref);
        lc_std_fun(std::, invoke);
    }
}
#endif

#ifndef KAIXO_LC_ANY
#define KAIXO_LC_ANY 1
#endif
#if KAIXO_LC_ANY == 1
#include <any>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, any_cast);
        lc_std_fun(std::, make_any);
    }
}
#endif

#ifndef KAIXO_LC_ALGORITHMS
#define KAIXO_LC_ALGORITHMS 1
#endif
#if KAIXO_LC_ALGORITHMS == 1
#include <algorithm>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, adjacent_find);
        lc_std_fun(std::, binary_search);
        lc_std_fun(std::, bsearch);
        lc_std_fun(std::, clamp);
        lc_std_fun(std::, copy_backward);
        lc_std_fun(std::, copy_n);
        lc_std_fun(std::, count);
        lc_std_fun(std::, count_if);
        lc_std_fun(std::, equal);
        lc_std_fun(std::, equal_range);
        lc_std_fun(std::, fill);
        lc_std_fun(std::, fill_n);
        lc_std_fun(std::, find);
        lc_std_fun(std::, find_end);
        lc_std_fun(std::, find_first_of);
        lc_std_fun(std::, find_if);
        lc_std_fun(std::, find_if_not);
        lc_std_fun(std::, for_each);
        lc_std_fun(std::, for_each_n);
        lc_std_fun(std::, generate);
        lc_std_fun(std::, generate_n);
        lc_std_fun(std::, includes);
        lc_std_fun(std::, inplace_merge);
        lc_std_fun(std::, iter_swap);
        lc_std_fun(std::, lexicographical_compare);
        lc_std_fun(std::, lower_bound);
        lc_std_fun(std::, make_heap);
        lc_std_fun(std::, max);
        lc_std_fun(std::, max_element);
        lc_std_fun(std::, merge);
        lc_std_fun(std::, min);
        lc_std_fun(std::, min_element);
        lc_std_fun(std::, minmax);
        lc_std_fun(std::, minmax_element);
        lc_std_fun(std::, mismatch);
        lc_std_fun(std::, move);
        lc_std_fun(std::, move_backward);
        lc_std_fun(std::, next_permutation);
        lc_std_fun(std::, nth_element);
        lc_std_fun(std::, partial_sort);
        lc_std_fun(std::, partial_sort_copy);
        lc_std_fun(std::, partition);
        lc_std_fun(std::, partition_copy);
        lc_std_fun(std::, partition_point);
        lc_std_fun(std::, pop_heap);
        lc_std_fun(std::, prev_permutation);
        lc_std_fun(std::, push_heap);
        lc_std_fun(std::, qsort);
        lc_std_fun(std::, remove);
        lc_std_fun(std::, remove_copy);
        lc_std_fun(std::, replace);
        lc_std_fun(std::, replace_copy);
        lc_std_fun(std::, replace_copy_if);
        lc_std_fun(std::, reverse);
        lc_std_fun(std::, reverse_copy);
        lc_std_fun(std::, rotate);
        lc_std_fun(std::, rotate_copy);
        lc_std_fun(std::, sample);
        lc_std_fun(std::, search);
        lc_std_fun(std::, search_n);
        lc_std_fun(std::, shift_left);
        lc_std_fun(std::, shift_right);
        lc_std_fun(std::, set_difference);
        lc_std_fun(std::, set_intersection);
        lc_std_fun(std::, set_symmetric_difference);
        lc_std_fun(std::, set_union);
        lc_std_fun(std::, sort);
        lc_std_fun(std::, sort_heap);
        lc_std_fun(std::, stable_partition);
        lc_std_fun(std::, stable_sort);
        lc_std_fun(std::, swap);
        lc_std_fun(std::, swap_ranges);
        lc_std_fun(std::, transform);
        lc_std_fun(std::, unique);
        lc_std_fun(std::, unique_copy);
        lc_std_fun(std::, upper_bound);
    }
}
#endif

#ifndef KAIXO_LC_ITERATOR
#define KAIXO_LC_ITERATOR 1
#endif
#if KAIXO_LC_ITERATOR == 1
#include <iterator>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, advance);
        lc_std_fun(std::, back_inserter);
        lc_std_fun(std::, begin);
        lc_std_fun(std::, data);
        lc_std_fun(std::, distance);
        lc_std_fun(std::, empty);
        lc_std_fun(std::, end);
        lc_std_fun(std::, front_inserter);
        lc_std_fun(std::, inserter);
        lc_std_fun(std::, make_move_iterator);
        lc_std_fun(std::, make_reverse_iterator);
        lc_std_fun(std::, next);
        lc_std_fun(std::, prev);
        lc_std_fun(std::, rbegin);
        lc_std_fun(std::, rend);
        lc_std_fun(std::, size);
    }
}
#endif

#ifndef KAIXO_LC_MEMORY
#define KAIXO_LC_MEMORY 1
#endif
#if KAIXO_LC_MEMORY == 1
#include <memory>
#include <memory_resource>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, addressof);
        lc_std_fun(std::, align);
        lc_std_fun(std::, assume_aligned);
        lc_std_fun(std::, calloc);
        lc_std_fun(std::, free);
        lc_std_fun(std::, malloc);
        lc_std_fun(std::, realloc);
        lc_std_fun(std::, destroy);
        lc_std_fun(std::, destroy_at);
        lc_std_fun(std::, destroy_n);
        lc_std_fun(std::pmr::, get_default_resource);
        lc_std_fun(std::, make_obj_using_allocator);
        lc_std_fun(std::pmr::, new_delete_resource);
        lc_std_fun(std::pmr::, null_memory_resource);
        lc_std_fun(std::pmr::, pool_options);
        lc_std_fun(std::pmr::, set_default_resource);
        lc_std_fun(std::, to_address);
        lc_std_fun(std::, uninitialized_construct_using_allocator);
        lc_std_fun(std::, uninitialized_copy);
        lc_std_fun(std::, uninitialized_copy_n);
        lc_std_fun(std::, uninitialized_default_construct);
        lc_std_fun(std::, uninitialized_default_construct_n);
        lc_std_fun(std::, uninitialized_fill);
        lc_std_fun(std::, uninitialized_fill_n);
        lc_std_fun(std::, uninitialized_move);
        lc_std_fun(std::, uninitialized_move_n);
        lc_std_fun(std::, uninitialized_value_construct);
        lc_std_fun(std::, uninitialized_value_construct_n);
    }
}
#endif

#ifndef KAIXO_LC_NUMERIC
#define KAIXO_LC_NUMERIC 1
#endif
#if KAIXO_LC_NUMERIC == 1
#include <numeric>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, accumulate);
        lc_std_fun(std::, adjacent_difference);
        lc_std_fun(std::, inclusive_scan);
        lc_std_fun(std::, inner_product);
        lc_std_fun(std::, iota);
        lc_std_fun(std::, reduce);
        lc_std_fun(std::, partial_sum);
        lc_std_fun(std::, transform_exclusive_scan);
        lc_std_fun(std::, transform_inclusive_scan);
        lc_std_fun(std::, transform_reduce);

        lc_std_fun(std::, bit_cast);
        lc_std_fun(std::, gcd);
        lc_std_fun(std::, lcm);
        lc_std_fun(std::, lerp);
        lc_std_fun(std::, abs);
        lc_std_fun(std::, acos);
        lc_std_fun(std::, acosh);
        lc_std_fun(std::, asin);
        lc_std_fun(std::, asinh);
        lc_std_fun(std::, atan);
        lc_std_fun(std::, atan2);
        lc_std_fun(std::, atanh);
        lc_std_fun(std::, cbrt);
        lc_std_fun(std::, ceil);
        lc_std_fun(std::, copysign);
        lc_std_fun(std::, cos);
        lc_std_fun(std::, cosh);
        lc_std_fun(std::, div);
        lc_std_fun(std::, erf);
        lc_std_fun(std::, erfc);
        lc_std_fun(std::, exp);
        lc_std_fun(std::, exp2);
        lc_std_fun(std::, expm1);
        lc_std_fun(std::, fabs);
        lc_std_fun(std::, fdim);
        lc_std_fun(std::, floor);
        lc_std_fun(std::, fma);
        lc_std_fun(std::, fmax);
        lc_std_fun(std::, fmin);
        lc_std_fun(std::, fmod);
        lc_std_fun(std::, fpclassify);
        lc_std_fun(std::, frexp);
        lc_std_fun(std::, hypot);
        lc_std_fun(std::, ilogb);
        lc_std_fun(std::, isfinite);
        lc_std_fun(std::, isgreater);
        lc_std_fun(std::, isgreaterequal);
        lc_std_fun(std::, isinf);
        lc_std_fun(std::, isless);
        lc_std_fun(std::, islessequal);
        lc_std_fun(std::, islessgreater);
        lc_std_fun(std::, isnan);
        lc_std_fun(std::, isnormal);
        lc_std_fun(std::, isunordered);
        lc_std_fun(std::, ldexp);
        lc_std_fun(std::, lgamma);
        lc_std_fun(std::, log);
        lc_std_fun(std::, log10);
        lc_std_fun(std::, log1p);
        lc_std_fun(std::, log2);
        lc_std_fun(std::, logb);
        lc_std_fun(std::, modf);
        lc_std_fun(std::, nan);
        lc_std_fun(std::, nearbyint);
        lc_std_fun(std::, nextafter);
        lc_std_fun(std::, pow);
        lc_std_fun(std::, remainder);
        lc_std_fun(std::, remquo);
        lc_std_fun(std::, rint);
        lc_std_fun(std::, round);
        lc_std_fun(std::, scalbn);
        lc_std_fun(std::, signbit);
        lc_std_fun(std::, sin);
        lc_std_fun(std::, sinh);
        lc_std_fun(std::, sqrt);
        lc_std_fun(std::, tan);
        lc_std_fun(std::, tanh);
        lc_std_fun(std::, tgamma);
        lc_std_fun(std::, trunc);
        lc_std_fun(std::, midpoint);
        lc_std_fun(std::, assoc_laguerre);
        lc_std_fun(std::, assoc_legendre);
        lc_std_fun(std::, beta);
        lc_std_fun(std::, comp_ellint_1);
        lc_std_fun(std::, comp_ellint_2);
        lc_std_fun(std::, comp_ellint_3);
        lc_std_fun(std::, cyl_bessel_i);
        lc_std_fun(std::, cyl_bessel_j);
        lc_std_fun(std::, cyl_bessel_k);
        lc_std_fun(std::, cyl_neumann);
        lc_std_fun(std::, ellint_1);
        lc_std_fun(std::, ellint_2);
        lc_std_fun(std::, ellint_3);
        lc_std_fun(std::, expint);
        lc_std_fun(std::, hermite);
        lc_std_fun(std::, laguerre);
        lc_std_fun(std::, legendre);
        lc_std_fun(std::, riemann_zeta);
        lc_std_fun(std::, sph_bessel);
        lc_std_fun(std::, sph_legendre);
        lc_std_fun(std::, sph_neumann);
    }
}
#endif

#ifndef KAIXO_LC_STRING
#define KAIXO_LC_STRING 1
#endif
#if KAIXO_LC_STRING == 1
#include <string>
#include <cstring>
#include <cwctype>
#include <cuchar>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, atof);
        lc_std_fun(std::, atoi);
        lc_std_fun(std::, isalnum);
        lc_std_fun(std::, isalpha);
        lc_std_fun(std::, isblank);
        lc_std_fun(std::, iscntrl);
        lc_std_fun(std::, isdigit);
        lc_std_fun(std::, isgraph);
        lc_std_fun(std::, islower);
        lc_std_fun(std::, isprint);
        lc_std_fun(std::, ispunct);
        lc_std_fun(std::, isspace);
        lc_std_fun(std::, isupper);
        lc_std_fun(std::, isxdigit);
        lc_std_fun(std::, memchr);
        lc_std_fun(std::, memcmp);
        lc_std_fun(std::, memcpy);
        lc_std_fun(std::, memmove);
        lc_std_fun(std::, memset);
        lc_std_fun(std::, strcat);
        lc_std_fun(std::, strchr);
        lc_std_fun(std::, strcmp);
        lc_std_fun(std::, strcoll);
        lc_std_fun(std::, strcpy);
        lc_std_fun(std::, strcspn);
        lc_std_fun(std::, strerror);
        lc_std_fun(std::, strlen);
        lc_std_fun(std::, strncat);
        lc_std_fun(std::, strncmp);
        lc_std_fun(std::, strncpy);
        lc_std_fun(std::, strpbrk);
        lc_std_fun(std::, strrchr);
        lc_std_fun(std::, strspn);
        lc_std_fun(std::, strstr);
        lc_std_fun(std::, strtof);
        lc_std_fun(std::, strtok);
        lc_std_fun(std::, strtol);
        lc_std_fun(std::, strtoul);
        lc_std_fun(std::, strxfrm);
        lc_std_fun(std::, tolower);
        lc_std_fun(std::, toupper);
        lc_std_fun(std::, copy);
        lc_std_fun(std::, btowc);
        lc_std_fun(std::, c16rtomb);
        lc_std_fun(std::, c32rtomb);
        lc_std_fun(std::, mblen);
        lc_std_fun(std::, mbrlen);
        lc_std_fun(std::, mbrtoc16);
        lc_std_fun(std::, mbrtoc32);
        lc_std_fun(std::, mbrtowc);
        lc_std_fun(std::, mbsinit);
        lc_std_fun(std::, mbsrtowcs);
        lc_std_fun(std::, mbstowcs);
        lc_std_fun(std::, mbtowc);
        lc_std_fun(std::, wcrtomb);
        lc_std_fun(std::, wcsrtombs);
        lc_std_fun(std::, wcstombs);
        lc_std_fun(std::, wctob);
        lc_std_fun(std::, wctomb);
        lc_std_fun(std::, iswalnum);
        lc_std_fun(std::, iswalpha);
        lc_std_fun(std::, iswblank);
        lc_std_fun(std::, iswcntrl);
        lc_std_fun(std::, iswctype);
        lc_std_fun(std::, iswdigit);
        lc_std_fun(std::, iswgraph);
        lc_std_fun(std::, iswlower);
        lc_std_fun(std::, iswprint);
        lc_std_fun(std::, iswpunct);
        lc_std_fun(std::, iswspace);
        lc_std_fun(std::, iswupper);
        lc_std_fun(std::, iswxdigit);
        lc_std_fun(std::, towctrans);
        lc_std_fun(std::, towlower);
        lc_std_fun(std::, towupper);
        lc_std_fun(std::, wcscat);
        lc_std_fun(std::, wcschr);
        lc_std_fun(std::, wcscmp);
        lc_std_fun(std::, wcscoll);
        lc_std_fun(std::, wcscpy);
        lc_std_fun(std::, wcscspn);
        lc_std_fun(std::, wcslen);
        lc_std_fun(std::, wcsncat);
        lc_std_fun(std::, wcsncmp);
        lc_std_fun(std::, wcsncpy);
        lc_std_fun(std::, wcspbrk);
        lc_std_fun(std::, wcsrchr);
        lc_std_fun(std::, wcsspn);
        lc_std_fun(std::, wcsstr);
        lc_std_fun(std::, wcstof);
        lc_std_fun(std::, wcstok);
        lc_std_fun(std::, wcstol);
        lc_std_fun(std::, wcstoul);
        lc_std_fun(std::, wcsxfrm);
        lc_std_fun(std::, wctrans);
        lc_std_fun(std::, wctype);
        lc_std_fun(std::, wmemchr);
        lc_std_fun(std::, wmemcmp);
        lc_std_fun(std::, wmemcpy);
        lc_std_fun(std::, wmemmove);
        lc_std_fun(std::, wmemset);
    }
}
#endif

#undef lc_std_fun
#undef lc_mem_fun
