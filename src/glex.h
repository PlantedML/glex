#ifndef GLEX_H_
#define GLEX_H_

#include <set>
#include <map>
#include <vector>

typedef std::map<std::set<unsigned int>, double> ProbsMap;
typedef std::map<std::set<unsigned int>, std::vector<unsigned int>> PathData;

struct AugmentedData
{
  std::set<unsigned int> encountered;
  PathData pathData;
};

struct LeafData
{
  std::set<unsigned int> all_encountered;
  std::map<unsigned int, std::set<unsigned int>> encountered;
  std::map<unsigned int, ProbsMap> leafProbs;
};

enum Index
{
  FEATURE = 0,
  SPLIT = 1,
  YES = 2,
  NO = 3,
  QUALITY = 4,
  COVER = 5
};

// Define ExtendedMask as a vector of uint64_t chunks for unlimited features
using ExtendedMask = std::vector<uint64_t>;

// Custom hash function for ExtendedMask
struct ExtendedMaskHash
{
  std::size_t operator()(const ExtendedMask &mask) const
  {
    std::size_t seed = 0;
    for (const auto &chunk : mask)
    {
      // Combine hash values (boost::hash_combine approach)
      seed ^= std::hash<uint64_t>{}(chunk) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
  }
};

// Custom equality function for ExtendedMask
struct ExtendedMaskEqual
{
  bool operator()(const ExtendedMask &lhs, const ExtendedMask &rhs) const
  {
    return lhs == rhs;
  }
};

// Extended bitmask data structures
using FeatureMask = ExtendedMask;
typedef std::unordered_map<FeatureMask, std::vector<unsigned int>, ExtendedMaskHash, ExtendedMaskEqual> PathDataBitmask;

struct AugmentedDataBitMask
{
  FeatureMask encountered; // vector of 64-bit masks for encountered features
  PathDataBitmask pathData;
};

struct LeafDataBitMask
{
  std::vector<FeatureMask> encounteredMask; // for each node
  std::vector<std::unordered_map<FeatureMask, double, ExtendedMaskHash, ExtendedMaskEqual>> leafProbs;
  FeatureMask allEncounteredMask; // union of all encountered features
};

// Constants for the implementation
constexpr unsigned int BITS_PER_CHUNK = 64;

// Create an empty mask with at least enough chunks for n_features
inline FeatureMask createEmptyMask(unsigned int n_features)
{
  unsigned int n_chunks = (n_features + BITS_PER_CHUNK - 1) / BITS_PER_CHUNK;
  return FeatureMask(n_chunks, 0ULL);
}

// Extended bitmask utility functions
inline bool hasFeature(const FeatureMask &mask, unsigned int feature)
{
  unsigned int chunk_idx = feature / BITS_PER_CHUNK;
  unsigned int bit_pos = feature % BITS_PER_CHUNK;

  return chunk_idx < mask.size() && (mask[chunk_idx] & (1ULL << bit_pos)) != 0ULL;
}

inline FeatureMask setFeature(FeatureMask mask, unsigned int feature)
{
  unsigned int chunk_idx = feature / BITS_PER_CHUNK;
  unsigned int bit_pos = feature % BITS_PER_CHUNK;

  // Resize if needed
  if (chunk_idx >= mask.size())
  {
    mask.resize(chunk_idx + 1, 0ULL);
  }

  mask[chunk_idx] |= (1ULL << bit_pos);
  return mask;
}

inline FeatureMask clearFeature(FeatureMask mask, unsigned int feature)
{
  unsigned int chunk_idx = feature / BITS_PER_CHUNK;
  unsigned int bit_pos = feature % BITS_PER_CHUNK;

  if (chunk_idx < mask.size())
  {
    mask[chunk_idx] &= ~(1ULL << bit_pos);
  }
  return mask;
}

// Bitwise AND between two masks
inline FeatureMask bitmaskAnd(const FeatureMask &a, const FeatureMask &b)
{
  size_t common_size = std::min(a.size(), b.size());
  FeatureMask result(common_size, 0ULL);

  for (size_t i = 0; i < common_size; i++)
  {
    result[i] = a[i] & b[i];
  }

  return result;
}

// Count set bits in a mask
inline unsigned int countSetBits(const FeatureMask &mask)
{
  unsigned int count = 0;
  for (const auto &chunk : mask)
  {
    count += __builtin_popcountll(chunk);
  }
  return count;
}

// Helper function to convert set to extended bitmask
inline FeatureMask setToBitmask(const std::set<unsigned int> &set)
{
  // Find the max feature to determine the required mask size
  unsigned int max_feature = 0;
  if (!set.empty())
  {
    max_feature = *set.rbegin(); // Largest element in the set
  }

  FeatureMask mask = createEmptyMask(max_feature + 1);
  for (unsigned int feature : set)
  {
    mask = setFeature(mask, feature);
  }
  return mask;
}

// Helper function to convert extended bitmask to set
inline std::set<unsigned int> bitmaskToSet(const FeatureMask &mask)
{
  std::set<unsigned int> set;

  for (size_t chunk_idx = 0; chunk_idx < mask.size(); chunk_idx++)
  {
    uint64_t chunk = mask[chunk_idx];
    if (chunk == 0)
      continue; // Skip empty chunks for efficiency

    for (unsigned int bit_pos = 0; bit_pos < BITS_PER_CHUNK; bit_pos++)
    {
      if (chunk & (1ULL << bit_pos))
      {
        unsigned int feature = chunk_idx * BITS_PER_CHUNK + bit_pos;
        set.insert(feature);
      }
    }
  }

  return set;
}

// Get all subsets of a given mask up to max_size, generating by size
inline std::vector<FeatureMask> get_all_subsets_of_mask(const FeatureMask &mask, unsigned int max_size = UINT_MAX)
{
  // 1) Gather indices of set bits
  std::vector<unsigned int> bits; // Stores the original indices of set bits in the mask

  for (size_t chunk_idx = 0; chunk_idx < mask.size(); chunk_idx++)
  {
    uint64_t chunk = mask[chunk_idx];
    if (chunk == 0)
      continue; // Skip empty chunks

    for (unsigned int bit_pos = 0; bit_pos < BITS_PER_CHUNK; bit_pos++)
    {
      if (chunk & (1ULL << bit_pos))
      {
        bits.push_back(chunk_idx * BITS_PER_CHUNK + bit_pos);
      }
    }
  }

  const size_t k = bits.size(); // Total number of set bits in the original mask (number of features to choose from)

  // 2) Generate subsets by size from 0 to min(k, max_size)
  std::vector<FeatureMask> subsets; // No pre-allocation needed, or a rough estimate could be used

  for (unsigned int m = 0; m <= std::min((unsigned int)k, max_size); ++m)
  {
    // Generate combinations of size m from k items (indices in 'bits')
    // Use bitmasks to represent combinations of the 'k' features.

    if (m == 0)
    {
      // The only combination of size 0 is the empty set
      subsets.push_back(createEmptyMask(BITS_PER_CHUNK * mask.size()));
      continue;
    }

    // Check if it's possible to have m bits set with k features
    if (m > k)
      continue;

    // Start with the smallest combination of size m: the first m bits set
    size_t combination_mask = (1ULL << m) - 1; // This mask represents which of the 'k' features are selected

    // Iterate through all combination masks with m bits set using Gosper's Hack
    while (combination_mask < (1ULL << k))
    {

      // Construct the FeatureMask for this subset using the original feature indices from 'bits'
      FeatureMask subset_mask = createEmptyMask(BITS_PER_CHUNK * mask.size()); // Corrected call
      for (size_t b = 0; b < k; ++b)
      {
        if (combination_mask & (1ULL << b))
        {
          // If the b-th bit is set in combination_mask, include the bits[b]-th feature
          subset_mask = setFeature(subset_mask, bits[b]);
        }
      }
      subsets.push_back(subset_mask);

      // Get the next combination mask with m bits set (Gosper's Hack)
      size_t c = combination_mask;                 // current combination
      size_t u = c & -c;                           // rightmost '1' bit
      size_t v = c + u;                            // adds 1 to the left of u
      combination_mask = v + (((c ^ v) / u) >> 2); // Gets the next combination in lexicographical order

      // Safety check for potential overflow or unexpected behavior, although should not be needed for valid k and m
      if (combination_mask == 0 && m > 0 && k >= m)
        break; // Break if wrapped around unexpectedly
    }
  }

  return subsets;
}

// Check if a mask is a subset of another mask
inline bool isSubset(const FeatureMask &subset, const FeatureMask &superset)
{
  // For each chunk in subset
  for (size_t i = 0; i < subset.size() && i < superset.size(); i++)
  {
    // If any bit in subset is not in superset, it's not a subset
    if ((subset[i] & ~superset[i]) != 0)
    {
      return false;
    }
  }

  // Check if subset has any bits set beyond superset's size
  for (size_t i = superset.size(); i < subset.size(); i++)
  {
    if (subset[i] != 0)
    {
      return false;
    }
  }

  return true;
}

#endif // GLEX_H_
