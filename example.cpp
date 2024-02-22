#include <iostream>
#include <string>
#include <source_location>
#include <vector>
#include <map>

#include "kaixo/type_utils.hpp"


// ------------------------------------------------

namespace inaetics {
    template<class Ty>
    using Sequence = std::vector<Ty>;
}

// ------------------------------------------------

namespace CDT {
    using Uint_32 = std::uint32_t;
}

// ------------------------------------------------

template<class Data>
struct AggregateData {

    // ------------------------------------------------

    std::optional<Data> value{};
    std::vector<AggregateData<Data>> recurse{};

    // ------------------------------------------------
    
    AggregateData<Data>& operator[](std::size_t index) {
        while (index < recurse.size()) recurse.emplace_back();
        return recurse[index];
    }

    // ------------------------------------------------

};

// ------------------------------------------------

template<class ...Args> // Get last type in template pack
using lastType = std::tuple_element_t<sizeof...(Args) - 1, std::tuple<Args...>>;

// ------------------------------------------------

template<class DataStructure, class ...Dimensions>
struct Aggregator {

    // ------------------------------------------------

    using PreviousEntryType = lastType<DataStructure, typename Dimensions::ThisEntryType...>;

    // ------------------------------------------------

    Aggregator(DataStructure& data)
        : m_Data(data) 
    {}

    // ------------------------------------------------

    template<class EntryType>
    auto dimension(
        std::map<CDT::Uint_32, EntryType> PreviousEntryType::*access,
        const inaetics::Sequence<CDT::Uint_32>& selectedIds,
        bool aggregate) 
    {
        struct Dimension {
            using ThisEntryType = EntryType;
            std::map<CDT::Uint_32, EntryType> PreviousEntryType::* access;
            const inaetics::Sequence<CDT::Uint_32>& selectedIds;
            bool aggregate;
        };
     
        return Aggregator<DataStructure, Dimensions..., Dimension>{
            m_Data,
            std::tuple_cat(m_Dimensions, std::tuple(Dimension{ access, selectedIds, aggregate }))
        };
    }

    // ------------------------------------------------
    
    template<class Data, class Lambda>
    AggregateData<Data> foreach(Lambda callback) {
        AggregateData<Data> result{};
        foreachImpl<0>(callback, result, &m_Data);
        return result;
    }

    // ------------------------------------------------

private:
    DataStructure& m_Data;
    std::tuple<Dimensions...> m_Dimensions{};

    // ------------------------------------------------

    Aggregator(DataStructure& data, std::tuple<Dimensions...>&& dims)
        : m_Data(data), m_Dimensions(std::move(dims))
    {}

    // ------------------------------------------------
    
    template<std::size_t I, class Lambda, class Data, class PreviousEntry, class ...IdEntryPairs>
    void foreachImpl(Lambda& callback, 
                     AggregateData<Data>& data,
                     const PreviousEntry* previousEntry,
                     IdEntryPairs... pairs) 
    {
        if constexpr (I == sizeof...(Dimensions)) {
            if (!data.value.has_value()) data.value.emplace();
            callback(pairs..., data.value.value()); // Call callback with id/entry pairs/aggregate data
        } else {
            using EntryType = typename std::tuple_element_t<I, std::tuple<Dimensions...>>::ThisEntryType;
            auto& dimension = std::get<I>(m_Dimensions);
            // Grab all the data from the dimension
            bool aggregate = dimension.aggregate;
            const inaetics::Sequence<CDT::Uint_32>& selectedIds = dimension.selectedIds;
            std::map<CDT::Uint_32, EntryType> PreviousEntry::* access = dimension.access;
            // Grab the next entries map if possible
            const std::map<CDT::Uint_32, EntryType>* nextEntries = nullptr;
            if (previousEntry) nextEntries = &(previousEntry->*access);

            std::size_t index = 0ull;
            auto recurse = [&](CDT::Uint_32 id, const EntryType* entry) {
                foreachImpl<I + 1>(
                    callback,                         // Forward the user-defined callback
                    aggregate ? data : data[index++], // Next dimension's aggregate data
                    entry,                            // Current entry
                    pairs...,                         // All previous id/entry pairs
                    id, entry);                       // next id/entry pair
            };

            if (!selectedIds.empty()) { // Use the selected ids
                for (auto& id : selectedIds) {
                    bool containsId = nextEntries && nextEntries->find(id) != nextEntries->end();
                    recurse(id, containsId ? &nextEntries->at(id) : nullptr);
                }
            } else if (nextEntries && !nextEntries->empty()) { // Use entries in DataStore
                for (auto& [id, entry] : *nextEntries) recurse(id, &entry);
            } else if (aggregate) {   // No ids, and no entries in DataStore, but aggregate
                recurse(0u, nullptr); // Recurse with no data; ignore this dimension
            }
        }
    }

    // ------------------------------------------------

    template<class, class ...>
    friend class Aggregator;

    // ------------------------------------------------

};

// ------------------------------------------------

struct Data {
    bool value = true;
};

struct YData {
    std::map<CDT::Uint_32, Data> data{};
};

struct XData {
    std::map<CDT::Uint_32, YData> ydata{};
};

struct MyData {
    std::map<CDT::Uint_32, XData> xdata{};
};

int main() {

    MyData data;

    inaetics::Sequence<CDT::Uint_32> xdataIds;
    inaetics::Sequence<CDT::Uint_32> ydataIds;
    inaetics::Sequence<CDT::Uint_32> dataIds;

    auto result = Aggregator(data)
        .dimension(&MyData::xdata, xdataIds, false)
        .dimension(&XData::ydata, ydataIds, false)
        .dimension(&YData::data, dataIds, false)
        .foreach<Data>([](
            CDT::Uint_32 xid, const XData* xdata,    
            CDT::Uint_32 yid, const YData* ydata,
            CDT::Uint_32 id,  const Data* data,
            Data& result) 
        {
            result.value &= data->value;
        });


}