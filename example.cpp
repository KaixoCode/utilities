#include <iostream>
#include <string>
#include <source_location>
#include <vector>
#include <map>

#include "pack_utils/pack_utils.hpp"

/**
 * Type manipulator:
 * x transform<T<...>>                      Transform type to T<Ty>
 * - conditional_transform<Filter, T<...>>  Conditionally transform to T<Ty> if match Filter
 * - tparams                                Get the template parameters of Ty
 * - copy_tparams<T<...>>                   Copy tparams to T<Tys...>
 * - uninstantiate                          Remove the template parameters from Ty
 * - instantiate<T<...>>                    Add template parameters to T<Tys...>
 * - reinstantiate<T>                       Replace the template parameters from T like T<Tys...>
 */

/**
 * Pack info:
 * x element<I>                             Get I'th element
 * x contains<Ty>                           Contains Ty
 * x contains_all<Tys...>                   Contains all of Tys...
 * x contains_any<Tys...>                   Contains any of Tys...
 * x count<Ty>                              Number of occurences of Ty
 * x count_all<Tys...>                      Number of occurences of all Tys...
 * x count_filter<Filter>                   Number of occurences that match Filter
 * x count_unique                           Number of unique types
 * x index<Ty>                              First index of Ty
 * x index_filter<Filter>                   First index that matches Filter
 * x indices<Tys...>                        All indices of all Tys...
 * x indices_filter<Filter>                 All indices that match Filter
 * x first_index<Tys...>                    First index of any in Tys...
 * x first_indices<Tys...>                  First indices of all Tys...
 */

/**
 * Pack manipulators:
 * x reverse                                Reverse pack
 * x unique                                 Only keep first occurence of type
 * - join                                   Flatten pack of packs
 * - split<Tys...>                          Split pack at all Tys... (consumes)
 * - split_after<Tys...>                    Split pack after all Tys... (does not consume)
 * - split_before<Tys...>                   Split pack before all Tys... (does not consume)
 * - split_filter<Filter>                   Split pack at all Filter matches (consumes)
 * - split_after_filter<Filter>             Split pack after all Filter matches (does not consume)
 * - split_before_filter<Filter>            Split pack before all Filter matches (does not consume)
 * x sub<A, B>                              Only keep indices between A and B
 * x take<I>                                Take first I types
 * x take_while<Filter>                     Take while Filter matches
 * x take_until<Filter>                     Take until Filter matches
 * x drop<I>                                Drop first I types
 * x drop_while<Filter>                     Drop while Filter matches
 * x drop_until<Filter>                     Drop until Filter matches
 * - take_last<I>                           Take last I types
 * - take_last_while<Filter>                Take last while Filter matches
 * - drop_last<I>                           Drop last I types
 * - drop_last_while<Filter>                Drop last while Filter matches
 * x remove<Ty>                             Remove type Ty
 * x remove_all<Tys...>                     Remove types Tys...
 * x append<Ty>                             Append type Ty
 * x append_all<Tys...>                     Append types Tys...
 * x prepend<Ty>                            Prepend type Ty
 * x prepend_all<Tys...>                    Prepend types Tys...
 * x keep<I>                                Only keep index I
 * x keep_all<Is...>                        Only keep indices Is...
 * x erase<I>                               Remove index I
 * x erase_all<Is...>                       Remove all indices Is...
 * x insert<I, Ty>                          Insert Ty at index I
 * x insert_all<I, Tys...>                  Insert types Tys... at index I
 * x swap<I, B>                             Swap index I with B
 * x swap_all<Is..., B>                     Swap indices Is... with B
 * x replace<A, B...>                       Replace all B... with A
 * x replace_filter<A, Filter>              Replace Filter matches with A
 * x filter<Filter>                         Only keep types that match Filter
 * x erase_filter<Filter>                   Only keep types that do not match Filter
 * x sort<Sorter>                           Sort the types using the Sorter
 */

/**
 * Pack combiners:
 * x concat<Tys...>                         Concat all packs Tys...
 * - zip<Tys...>                            Zip all packs Tys...
 * - cartesian<Tys...>                      Cartesian product of all packs Tys...
 *
 */

 // ------------------------------------------------

int main() {
    using namespace kaixo;

}